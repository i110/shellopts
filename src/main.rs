use clap::{Arg, ArgMatches, Command, Parser};
use regex::Regex;
use serde::{de::Visitor, Deserialize, Deserializer};
use serde_json;
use shell_quote::Bash;
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::io::BufReader;

// static POSITIONAL_ARGUMENTS_ID: &str = "positional_arguments";

#[derive(Debug, Clone)]
enum Validator {
    Bool,
    String,
    Int,
    UInt,
    Number,
    Regexp(Regex),
    File,
}

impl Validator {
    fn value_name(&self) -> Option<&str> {
        Some(match self {
            Validator::Bool => return None,
            Validator::String => "VALUE",
            Validator::Int => "INT",
            Validator::UInt => "UINT",
            Validator::Number => "NUMBER",
            Validator::Regexp(_) => "VALUE",
            Validator::File => "FILE",
        })
    }
}

struct ValueTypeVisitor;

impl<'de> Visitor<'de> for ValueTypeVisitor {
    type Value = Validator;

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(match v {
            "bool" => Validator::Bool,
            "string" => Validator::String,
            "int" => Validator::Int,
            "uint" => Validator::UInt,
            "number" => Validator::Number,
            "file" => Validator::File,
            _ if v.starts_with("regexp") => {
                let re = Regex::new(r"^regexp/([^/]+)/$").unwrap();
                let m = if let Some(m) = re.captures(v) {
                    m
                } else {
                    return Err(E::custom(format!("invalid regexp: {}", v)));
                };
                let pat = m.get(1).unwrap();
                let regexp = Regex::new(pat.as_str())
                    .map_err(|e| E::custom(format!("inbalid regexp pattern: {}", e)))?;
                Validator::Regexp(regexp)
            }
            _ => return Err(E::custom(format!("unknown value type: {}", v))),
        })
    }

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("value type")
    }
}

impl<'de> Deserialize<'de> for Validator {
    fn deserialize<D>(deserializer: D) -> Result<Validator, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(ValueTypeVisitor)
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct AppOpts {
    #[arg(short, long)]
    name: Option<String>,
    #[arg(short, long)]
    schema: String,
    #[arg(long)]
    varname_prefix: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
struct SchemaOpt {
    name: String,
    short: Option<String>,

    #[serde(default)]
    takes_value: bool,

    value_name: Option<String>,
    validator: Option<Validator>,

    #[serde(default)]
    required: bool,

    default_value: Option<String>,
    env: Option<String>,
    help: Option<String>,
}

impl SchemaOpt {
    fn value_name(&self) -> Option<&str> {
        if !self.takes_value {
            return None;
        }
        if self.value_name.is_some() {
            return self.value_name.as_deref();
        }
        if let Some(val) = self.validator.as_ref() {
            return val.value_name();
        }
        return Some("VALUE");
    }

    fn is_hit(&self, arg: &str) -> bool {
        if let Some(short) = self.short.as_ref() {
            if self.takes_value {
                if arg.starts_with(&format!("-{}=", short)) {
                    return true;
                }
            } else {
                if arg == format!("-{}", short) {
                    return true;
                }
            }
        }
        if self.takes_value {
            if arg.starts_with(&format!("--{}=", self.name)) {
                return true;
            }
        } else {
            if arg == format!("--{}", self.name) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Deserialize)]
struct Schema {
    #[serde(default)]
    ignore_unknown_options: bool,
    options: Vec<SchemaOpt>,
}

fn build_command(schema: &Schema, app_name: &str) -> Command {
    let mut command = Command::new(app_name.to_owned())
        .no_binary_name(true)
        .disable_version_flag(true);
    for option in schema.options.iter() {
        if option.name == "help" {
            continue; // help is auto generated, ignore
        }

        let mut arg = Arg::new(&option.name);
        arg = arg.long(&option.name);
        if let Some(short) = option.short.as_ref() {
            arg = arg.short(short.chars().next().expect("empty string"));
        }
        if let Some(value_name) = option.value_name() {
            arg = arg.value_name(value_name.to_string());
        }
        arg = arg.num_args(if option.takes_value { 1 } else { 0 });
        arg = arg.required(option.required);
        if let Some(env) = option.env.as_ref() {
            arg = arg.env(env);
        }
        if let Some(default_value) = option.default_value.as_ref() {
            arg = arg.default_value(default_value);
        }
        if let Some(help) = option.help.as_ref() {
            arg = arg.help(help);
        }

        command = command.arg(arg);
    }
    // command = command.arg(Arg::new(POSITIONAL_ARGUMENTS_ID).num_args(0..).hide(true));
    command
}

struct Parsed<'a> {
    options: Vec<(&'a SchemaOpt, String)>,
    arguments: Vec<String>,
}

fn generate_script(parsed: Parsed, app_opts: AppOpts) -> anyhow::Result<String> {
    let mut buffer = String::new();

    for (opt, value) in parsed.options {
        let varname = if let Some(varname_prefix) = app_opts.varname_prefix.as_ref() {
            format!("{}_{}", varname_prefix, opt.name.to_uppercase())
        } else {
            opt.name.as_str().to_uppercase()
        };

        let escaped = String::from_utf8(Bash::quote_vec(&value))?;
        writeln!(&mut buffer, "export {}={}", varname, escaped).unwrap();
    }

    if parsed.arguments.len() != 0 {
        writeln!(&mut buffer, "set -- {}", parsed.arguments.join(" "))?;
    }
    Ok(buffer)
}

fn generate_parsed<'a>(schema: &'a Schema, m: ArgMatches, pargs: Vec<&str>) -> Parsed<'a> {
    let mut parsed = Parsed {
        options: vec![],
        arguments: pargs.into_iter().map(|x| x.to_string()).collect(),
    };

    for id in m.ids() {
        let opt = if let Some(opt) = schema.options.iter().find(|opt| opt.name == id.as_str()) {
            opt
        } else {
            continue;
        };

        let value = if let Some(value) = if opt.takes_value {
            Some(m.get_one::<String>(id.as_str()).map(|x| x.as_str()))
        } else {
            m.get_one::<bool>(id.as_str())
                .map(|&x| if x { Some("1") } else { None })
        } {
            value
        } else {
            continue;
        };

        if let Some(value) = value {
            parsed.options.push((opt, value.to_owned()));
        }
    }

    parsed
}

fn split_into_options_and_positional_arguments<'a>(
    schema: &Schema,
    args: &[&'a str],
) -> (Vec<&'a str>, Vec<&'a str>) {
    let mut options = vec![];
    let mut pargs = vec![];
    for &arg in args {
        let opt = schema.options.iter().find(|opt| opt.is_hit(arg));
        if arg.starts_with("-") {
            if schema.ignore_unknown_options && opt.is_none() {
                pargs.push(arg);
            } else {
                options.push(arg);
            }
        } else {
            pargs.push(arg);
        }
    }
    (options, pargs)
}

fn parse<'a>(schema: &'a Schema, app_name: &str, args: &[&str]) -> anyhow::Result<Parsed<'a>> {
    let (option_args, pargs) = split_into_options_and_positional_arguments(schema, args);

    let command = build_command(schema, app_name);
    let m = command.try_get_matches_from(option_args)?;
    Ok(generate_parsed(&schema, m, pargs))
}

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args_os()
        .map(|x| x.into_string())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| anyhow::Error::msg("invalid utf-8 sequence"))?;
    let mut splitted = args.split(|arg| arg == "--");
    let app_args = splitted.next().unwrap_or_default();
    let target_args = splitted
        .next()
        .unwrap_or_default()
        .into_iter()
        .map(|x| x.as_str())
        .collect::<Vec<_>>();

    let app_opts = AppOpts::parse_from(app_args);

    let schema: Schema = if app_opts.schema.starts_with("@") {
        let file = File::open(app_opts.schema.chars().skip(1).collect::<String>())?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader)?
    } else {
        serde_json::from_str(&app_opts.schema)?
    };

    let parsed = parse(
        &schema,
        app_opts.name.as_deref().unwrap_or("PROGRAM"),
        &target_args,
    )?;

    let script = generate_script(parsed, app_opts)?;
    println!("{}", script);

    anyhow::Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let mut schema = Schema {
            ignore_unknown_options: true,
            options: vec![
                SchemaOpt {
                    name: "foo".into(),
                    takes_value: true,
                    required: true,
                    ..Default::default()
                },
                SchemaOpt {
                    name: "bar".into(),
                    ..Default::default()
                },
            ],
        };
        let parsed = parse(&schema, "", &["--foo=XXX".into(), "--bar".into()]).unwrap();
        assert_eq!(parsed.options.len(), 2);
        assert_eq!(parsed.options[0].1, "XXX");
        assert_eq!(parsed.options[1].1, "1");

        let parsed = parse(&schema, "", &["--foo=XXX".into()]).unwrap();
        assert_eq!(
            parsed.options.len(),
            1,
            "no required flag doesn't appear in parsed"
        );

        let parsed = parse(
            &schema,
            "",
            &[
                "aaa".into(),
                "--unknown1".into(),
                "--foo=XXX".into(),
                "-u".into(),
                "--bar".into(),
                "bbb".into(),
                "--unknown3".into(),
            ],
        )
        .unwrap();
        assert_eq!(parsed.options.len(), 2);
        assert_eq!(parsed.options[0].1, "XXX");
        assert_eq!(parsed.options[1].1, "1");
        assert_eq!(parsed.arguments.len(), 5);

        schema.ignore_unknown_options = false;
        let parsed = parse(
            &schema,
            "",
            &[
                "aaa".into(),
                "--unknown1".into(),
                "--foo=XXX".into(),
                "-u".into(),
                "--bar".into(),
                "bbb".into(),
                "--unknown3".into(),
            ],
        );
        assert!(parsed.is_err());
        assert!(parsed
            .err()
            .unwrap()
            .to_string()
            .starts_with("error: unexpected argument '--unknown1' found"));
    }
}

use clap::{Arg, ArgMatches, Command, Parser};
use regex::Regex;
use serde::{de::Visitor, Deserialize, Deserializer};
use serde_json;
use shell_quote::Bash;
use std::env;
use std::ffi::OsString;
use std::fmt::Write;
use std::fs::File;
use std::io::BufReader;

static POSITIONAL_ARGUMENTS_ID: &str = "positional_arguments";

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
    schema: String,
    #[arg(long)]
    varname_prefix: Option<String>,
}

#[derive(Debug, Deserialize)]
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
}

#[derive(Debug, Deserialize)]
struct Schema {
    name: Option<String>,
    options: Vec<SchemaOpt>,
}

fn build_command(schema: &Schema) -> Command {
    let mut command = Command::new(schema.name.clone().unwrap_or("hoge".into()))
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
    command = command.arg(Arg::new(POSITIONAL_ARGUMENTS_ID).num_args(0..).hide(true));
    command
}

struct Parsed<'a> {
    options: Vec<(&'a SchemaOpt, &'a str)>,
    arguments: Vec<&'a str>,
}

fn generate_script(parsed: Parsed, app_opts: AppOpts) -> anyhow::Result<String> {
    let mut buffer = String::new();

    for (opt, value) in parsed.options {
        let varname = if let Some(varname_prefix) = app_opts.varname_prefix.as_ref() {
            format!("{}_{}", varname_prefix, opt.name.to_uppercase())
        } else {
            opt.name.as_str().to_uppercase()
        };

        let escaped = String::from_utf8(Bash::quote_vec(value))?;
        writeln!(&mut buffer, "export {}={}", varname, escaped).unwrap();
    }

    if parsed.arguments.len() != 0 {
        writeln!(&mut buffer, "set -- {}", parsed.arguments.join(" "))?;
    }
    Ok(buffer)
}

fn parse<'a>(schema: &'a Schema, m: &'a ArgMatches) -> Parsed<'a> {
    let mut parsed = Parsed {
        options: vec![],
        arguments: vec![],
    };

    for id in m.ids() {
        let opt = if let Some(opt) = schema.options.iter().find(|opt| opt.name == id.as_str()) {
            opt
        } else {
            continue;
        };

        let value = if let Some(value) = if opt.takes_value {
            m.get_one::<String>(id.as_str()).map(|x| x.as_str())
        } else {
            m.get_one::<bool>(id.as_str())
                .map(|&x| if x { "1" } else { "0" })
        } {
            value
        } else {
            continue;
        };

        parsed.options.push((opt, value));
    }

    if let Some(args) = m.get_many::<String>(POSITIONAL_ARGUMENTS_ID) {
        for arg in args {
            parsed.arguments.push(arg);
        }
    }

    parsed
}

fn main() -> anyhow::Result<()> {
    let args: Vec<OsString> = env::args_os().collect();
    let mut splitted = args.split(|arg| arg == "--");
    let app_args = splitted.next().unwrap_or_default();
    let target_args = splitted.next().unwrap_or_default();

    let app_opts = AppOpts::parse_from(app_args);

    let schema: Schema = if app_opts.schema.starts_with("@") {
        let file = File::open(app_opts.schema.chars().skip(1).collect::<String>())?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader)?
    } else {
        serde_json::from_str(&app_opts.schema)?
    };

    let command = build_command(&schema);
    let m = command.try_get_matches_from(target_args)?;

    let parsed = parse(&schema, &m);
    let script = generate_script(parsed, app_opts)?;
    println!("{}", script);

    anyhow::Ok(())
}

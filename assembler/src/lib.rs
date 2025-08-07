use crate::assembler::{Assembler, lang::AssemblyLanguage};

use crate::config::AssemblerConfig;
use crate::context::Context;
use crate::logs::LogEntry;
use crate::preprocess::PreProcessor;
use bumpalo::Bump;
use std::collections::HashMap;
use std::time::Instant;

pub mod assembler;
pub mod expression;
pub mod lex;
pub mod preprocess;
pub mod simple;

pub mod config;
pub mod context;
pub mod logs;
pub mod util;

pub struct AssemblerResult<'a> {
    pub time: f64,
    pub allocated: usize,
    pub output: Vec<u8>,
    pub log: Vec<LogEntry<'a>>,
}

impl<'a> std::fmt::Display for AssemblerResult<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut errors = 0;
        let mut warnings = 0;
        for log in &self.log {
            errors += log
                .parts
                .iter()
                .filter(|t| t.kind == LogKind::Error)
                .count();
            warnings += log
                .parts
                .iter()
                .filter(|t| t.kind == LogKind::Warning)
                .count();
            writeln!(f, "{log}")?;
        }

        use logs::*;

        if warnings > 0 {
            writeln!(
                f,
                "{BOLD}{YELLOW}warning{RESET}{BOLD}: {warnings} warning(s) emitted{RESET}"
            )?;
        }
        if errors > 0 {
            writeln!(
                f,
                "{BOLD}{RED}error{RESET}{BOLD}: could not assemble due to {errors} error(s). took {}s allocated {}b{RESET}",
                self.time, self.allocated
            )
        } else {
            writeln!(
                f,
                "{BOLD}{GREEN}Finished{RESET}{BOLD} in {}s allocated {}b{RESET}",
                self.time, self.allocated
            )
        }
    }
}

pub fn with_bump<R>(func: impl FnOnce(&Bump) -> R) -> R {
    func(&Bump::new())
}

pub fn assemble_and_link<'a>(
    sources: &'a HashMap<String, String>,
    files: Vec<impl Into<String>>,
    bump: &'a Bump,
    mut lang: impl AssemblyLanguage<'a>,
) -> AssemblerResult<'a> {
    let now = Instant::now();
    let mut context = Context::new(bump, AssemblerConfig::new(), move |path, _ctx| {
        if let Some(contents) = sources.get(path) {
            Ok(contents.as_str())
        } else {
            Err(format!("No source found with path '{path}'").into())
        }
    });
    let mut preprocessor = PreProcessor::new();

    let mut assember = Assembler::new(&mut context, &mut lang, &mut preprocessor);

    for file in files {
        assember.assemble(file);
    }

    let elapsed = now.elapsed().as_secs_f64();

    AssemblerResult {
        allocated: bump.allocated_bytes(),
        time: elapsed,
        output: Vec::new(),
        log: context.take_logs(),
    }
}

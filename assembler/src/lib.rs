use crate::assembler::Assembler;
use crate::context::Context;
use crate::preprocess::PreProcessor;
use bumpalo::Bump;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;

pub mod assembler;
pub mod context;
pub mod error;
pub mod lex;
pub mod preprocess;
mod util;

pub struct AssemblerResult {
    pub time: f64,
    pub allocated: usize,
    pub output: (),
    pub result: Result<(), ()>,
}

pub fn assemble_and_link(
    sources: HashMap<String, String>,
    files: Vec<impl Into<String>>,
) -> AssemblerResult {
    let now = Instant::now();
    let bump = Bump::new();
    let context = Rc::new(Context::new(&bump, move |path, _ctx| {
        if let Some(contents) = sources.get(path) {
            Ok(contents.to_owned())
        } else {
            Err(format!("No source found with path '{path}'").into())
        }
    }));
    let preprocessor = PreProcessor::new(context.clone());
    let mut assember = Assembler::new(context.clone(), preprocessor);

    for file in files {
        assember.assemble(file);
    }

    let elapsed = now.elapsed().as_secs_f64();
    context.print_errors();

    println!(
        "Finished in {:.3}s allocated {}b",
        elapsed,
        bump.allocated_bytes()
    );

    AssemblerResult {
        allocated: bump.allocated_bytes(),
        time: elapsed,
        output: (),
        result: Ok(()),
    }
}

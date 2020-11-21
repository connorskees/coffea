use std::{
    collections::HashMap,
    io::{BufWriter, Write},
    string::ToString,
};

use crate::{code::Instruction, errors::JResult};

#[derive(Debug, Clone)]
pub(crate) struct InstructionNode {
    pub pos: usize,
    pub inst: Instruction,
}

#[derive(Debug, Clone)]
pub(crate) struct ControlFlowGraph {
    edges: HashMap<usize, Vec<usize>>,
    nodes: HashMap<usize, Vec<InstructionNode>>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            edges: HashMap::new(),
            nodes: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, pos: usize, inst: Vec<InstructionNode>) {
        self.nodes.insert(pos, inst);
    }

    pub fn add_edge(&mut self, starting_pos: usize, ending_pos: usize) {
        self.edges.entry(starting_pos).or_default().push(ending_pos);
    }

    /// Serializes graph in DOT file format
    pub fn visualize(&self) -> JResult<()> {
        let mut file = BufWriter::new(
            std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .open("graph.dot")?,
        );

        writeln!(file, "digraph {{")?;
        writeln!(file, "  node [shape=record fontname=Arial];")?;
        writeln!(file)?;

        for (node, instructions) in &self.nodes {
            writeln!(
                file,
                "  {} [label=\"{{{}|{}}}\"]",
                node,
                node,
                instructions
                    .iter()
                    .map(|i| format!("{:?}", i.inst)
                        .replace('{', "\\{")
                        .replace('}', "\\}"))
                    .collect::<Vec<String>>()
                    .join("\\n")
            )?;
        }

        writeln!(file)?;

        for (node, edges) in &self.edges {
            writeln!(
                file,
                "  {} -> {{ {} }}",
                node,
                edges
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )?;
        }

        writeln!(file, "}}")?;

        file.flush()?;

        Ok(())
    }
}

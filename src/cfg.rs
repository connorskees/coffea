use std::{
    collections::{HashMap, HashSet},
    io::{BufWriter, Write},
    mem,
    string::ToString,
};

use crate::{
    errors::JResult,
    instructions::{Instruction, Instructions},
};

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
    pub fn new(tokens: &mut Instructions) -> Self {
        let mut graph = Self {
            edges: HashMap::new(),
            nodes: HashMap::new(),
        };

        let (instructions, block_starts) = Self::find_block_starts(tokens);

        let mut current_pos = 0;

        let mut current_block = Vec::new();

        let mut current_block_pos = None;

        let mut instructions = instructions.into_iter().peekable();

        while let Some(inst) = instructions.next() {
            current_block_pos = current_block_pos.or(Some(current_pos));

            match inst {
                Instruction::Goto(offset) => {
                    let pos_to = (current_pos as i64 + offset as i64) as usize;
                    graph.add_edge(current_block_pos.unwrap(), pos_to);
                }
                Instruction::GotoW(..) => todo!(),
                Instruction::IfAcmpeq(offset)
                | Instruction::IfAcmpne(offset)
                | Instruction::IfIcmpeq(offset)
                | Instruction::IfIcmpge(offset)
                | Instruction::IfIcmpgt(offset)
                | Instruction::IfIcmple(offset)
                | Instruction::IfIcmplt(offset)
                | Instruction::IfIcmpne(offset)
                | Instruction::Ifeq(offset)
                | Instruction::Ifge(offset)
                | Instruction::Ifgt(offset)
                | Instruction::Ifle(offset)
                | Instruction::Iflt(offset)
                | Instruction::Ifne(offset)
                | Instruction::Ifnonnull(offset)
                | Instruction::Ifnull(offset) => {
                    let pos_to = (current_pos as i64 + offset as i64) as usize;
                    graph.add_edge(current_block_pos.unwrap(), pos_to);
                    graph.add_edge(
                        current_block_pos.unwrap(),
                        current_pos + inst.len() as usize,
                    );
                }
                _ => {}
            }

            current_pos += inst.len() as usize;
            current_block.push(InstructionNode {
                inst,
                pos: current_pos as usize,
            });

            if block_starts.contains(&current_pos) {
                graph.add_node(current_block_pos.unwrap(), mem::take(&mut current_block));
                current_block_pos = None;
            }

            if let (Some(next_inst), Some(block_pos)) = (instructions.peek(), current_block_pos) {
                if block_starts.contains(&(current_pos + next_inst.len() as usize))
                    && !next_inst.is_control_flow()
                {
                    graph.add_edge(block_pos, current_pos + next_inst.len() as usize)
                }
            }
        }

        if let Some(pos) = current_block_pos {
            if !current_block.is_empty() {
                graph.add_node(pos, current_block);
            }
        }

        graph
    }

    pub fn find_block_starts(tokens: &mut Instructions) -> (Vec<Instruction>, HashSet<usize>) {
        let mut block_starts = HashSet::new();
        let mut instructions = Vec::new();

        let mut current_pos = 0;

        block_starts.insert(0);

        let mut current_block_pos = None;
        while let Some(inst) = tokens.next() {
            current_block_pos = current_block_pos.or(Some(current_pos));

            match inst {
                Instruction::Goto(offset) => {
                    let pos_to = (current_pos as i64 + offset as i64) as usize;
                    block_starts.insert(pos_to);
                }
                Instruction::GotoW(..) => todo!(),
                Instruction::IfAcmpeq(offset)
                | Instruction::IfAcmpne(offset)
                | Instruction::IfIcmpeq(offset)
                | Instruction::IfIcmpge(offset)
                | Instruction::IfIcmpgt(offset)
                | Instruction::IfIcmple(offset)
                | Instruction::IfIcmplt(offset)
                | Instruction::IfIcmpne(offset)
                | Instruction::Ifeq(offset)
                | Instruction::Ifge(offset)
                | Instruction::Ifgt(offset)
                | Instruction::Ifle(offset)
                | Instruction::Iflt(offset)
                | Instruction::Ifne(offset)
                | Instruction::Ifnonnull(offset)
                | Instruction::Ifnull(offset) => {
                    let pos_to = (current_pos as i64 + offset as i64) as usize;
                    block_starts.insert(pos_to);
                    block_starts.insert(current_pos + inst.len() as usize);
                }
                _ => {}
            }

            if inst.is_control_flow() {
                current_block_pos = None;
            }

            instructions.push(inst);

            current_pos += inst.len() as usize;
        }

        (instructions, block_starts)
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
                .truncate(true)
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
                    .map(|i| format!("{:?}", i.inst))
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

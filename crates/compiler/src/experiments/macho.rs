use crate::experiments::bc_to_asm::Jitted;
use crate::pool::{Ident, StringPool};

pub struct BuildExec<'p> {
    _pool: &'p StringPool<'p>,
    dyn_link: Vec<Ident<'p>>,
}

impl<'p> BuildExec<'p> {
    pub fn import_dynamic(&mut self, name: Ident<'p>) -> usize {
        if let Some(i) = self.dyn_link.iter().position(|c| *c == name) {
            return i;
        }
        self.dyn_link.push(name);
        self.dyn_link.len() - 1
    }

    pub fn build(self, _jitted: Jitted) -> Vec<u8> {
        let out = vec![];
        out
    }
}


pub struct Segment {
    pub name: &'static [u8],
    pub v_addr: usize,
    pub v_size: usize, // if larger than bytes of all sections, loader pads with zeros.
    pub sections: Vec<Section>
}

pub struct Section {
    pub name: &'static [u8],
    // must fit inside the segment and not overlap.
    pub v_addr: usize,
    pub v_size: usize, // if larger than bytes of all sections, loader pads with zeros.

}

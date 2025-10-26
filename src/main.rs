mod chunk;

fn main() {
    let mut chunk = chunk::Chunk::new();
    chunk.write_chunk(chunk::OpCode::OpReturn, 0);
    let index = chunk.add_constant(1.2);
    chunk.write_chunk(chunk::OpCode::OpConstant(index), 0);
    chunk.disassemble_chunk("Test Chunk");
}

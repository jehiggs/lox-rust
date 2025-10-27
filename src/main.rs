mod chunk;

fn main() {
    let mut chunk = chunk::Chunk::new();
    chunk.write_chunk(chunk::OpCode::Return, 0);
    chunk.write_constant(1.2, 0);
    chunk.disassemble_chunk("Test Chunk");
}

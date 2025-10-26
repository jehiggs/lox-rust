mod chunk;

fn main() {
    let mut chunk = chunk::Chunk::new();
    chunk.write_byte(chunk::OpCode::OpReturn);
    chunk.disassemble_chunk("Test Chunk");
}

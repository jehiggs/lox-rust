mod chunk;
mod vm;

fn main() {
    let mut chunk = chunk::Chunk::new();
    chunk.write_constant(1.2, 0);
    chunk.write_constant(3.4, 0);
    chunk.write_chunk(chunk::OpCode::Add, 0);
    chunk.write_constant(5.6, 0);
    chunk.write_chunk(chunk::OpCode::Divide, 0);
    chunk.write_chunk(chunk::OpCode::Negate, 0);
    chunk.write_chunk(chunk::OpCode::Return, 0);
    let mut vm = vm::VM::new(chunk);
    vm.interpret();
}

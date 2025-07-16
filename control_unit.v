module Control_Unit (
    input wire clk,
    input wire rst_n,
    input wire [6:0] instruction_opcode,
    output reg pc_write,
    output reg ir_write,
    output reg pc_source,
    output reg reg_write,
    output reg memory_read,
    output reg is_immediate,
    output reg memory_write,
    output reg pc_write_cond,
    output reg lorD,
    output reg memory_to_reg,
    output reg [1:0] aluop,
    output reg [1:0] alu_src_a,
    output reg [1:0] alu_src_b
);

	// machine states
	localparam FETCH     = 4'b0000;
	localparam DECODE    = 4'b0001;
	localparam MEMADR    = 4'b0010;
	localparam MEMREAD   = 4'b0011;
	localparam MEMWB     = 4'b0100;
	localparam MEMWRITE  = 4'b0101;
	localparam EXECUTER  = 4'b0110;
	localparam ALUWB     = 4'b0111;
	localparam EXECUTEI  = 4'b1000;
	localparam JAL       = 4'b1001;
	localparam BRANCH    = 4'b1010;
	localparam JALR      = 4'b1011;
	localparam AUIPC     = 4'b1100;
	localparam LUI       = 4'b1101;

	// Instruction Opcodes
	localparam LW      = 7'b0000011;
	localparam SW      = 7'b0100011;
	localparam RTYPE   = 7'b0110011;
	localparam ITYPE   = 7'b0010011;
	localparam JALI    = 7'b1101111;
	localparam BRANCHI = 7'b1100011;
	localparam JALRI   = 7'b1100111;
	localparam AUIPCI  = 7'b0010111;
	localparam LUII    = 7'b0110111;

    reg [3:0] state_cs;
    reg [3:0] state_ns;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            state_cs <= FETCH;
        else
            state_cs <= state_ns;
    end

    always @(*) begin
        // Valores padrão
        state_ns        = state_cs;
        pc_write        = 0;
        ir_write        = 0;
        pc_source       = 0;
        reg_write       = 0;
        memory_read     = 0;
        is_immediate    = 0;
        memory_write    = 0;
        pc_write_cond   = 0;
        lorD            = 0;
        memory_to_reg   = 0;
        aluop           = 2'b00;
        alu_src_a       = 2'b00;
        alu_src_b       = 2'b00;

        case (state_cs)
            FETCH: begin
                pc_write    = 1;
                ir_write    = 1;
                memory_read = 1;
                alu_src_a   = 2'b00;
                alu_src_b   = 2'b01;
                state_ns    = DECODE;
            end

            DECODE: begin
                alu_src_a = 2'b10;
                alu_src_b = 2'b10;
                case (instruction_opcode)
                    RTYPE:   state_ns = EXECUTER;
                    ITYPE:   state_ns = EXECUTEI;
                    LW:      state_ns = MEMADR;
                    SW:      state_ns = MEMADR;
                    JALI:    state_ns = JAL;
                    BRANCHI: state_ns = BRANCH;
                    JALRI:   state_ns = JALR;
                    AUIPCI:  state_ns = AUIPC;
                    LUII:    state_ns = LUI;
                    default: state_ns = FETCH;
                endcase
            end

            MEMADR: begin
                aluop        = 2'b00;
                alu_src_a    = 2'b01;
                alu_src_b    = 2'b01;
                is_immediate = 1;
                lorD         = 1;
                state_ns     = (instruction_opcode == LW) ? MEMREAD :
                               (instruction_opcode == SW) ? MEMWRITE : FETCH;
            end

            MEMREAD: begin
                memory_read = 1;
                lorD        = 1;
                state_ns    = MEMWB;
            end

            MEMWB: begin
                reg_write     = 1;
                memory_to_reg = 1;
                state_ns      = FETCH;
            end

            MEMWRITE: begin
                memory_write = 1;
                lorD         = 1;
                state_ns     = FETCH;
            end

            EXECUTER: begin
                aluop     = 2'b10;
                alu_src_a = 2'b01;
                alu_src_b = 2'b00;
                state_ns  = ALUWB;
            end

            EXECUTEI: begin
                aluop        = 2'b10;
                alu_src_a    = 2'b01;
                alu_src_b    = 2'b10;
                is_immediate = 1;
                state_ns     = ALUWB;
            end

            ALUWB: begin
                reg_write     = 1;
                memory_to_reg = 0;
                state_ns      = FETCH;
            end

            JAL: begin
                reg_write = 1;
                pc_write  = 1;
                pc_source = 1;
                state_ns  = FETCH;
            end

            BRANCH: begin
                pc_write_cond = 1;
                pc_source     = 1;
                aluop         = 2'b01;
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b00;
                state_ns      = FETCH;
            end

            JALR: begin
                aluop        = 2'b00;
                alu_src_a    = 2'b10;
                alu_src_b    = 2'b01;
                pc_source    = 2'b10;
                pc_write     = 1;
                is_immediate = 1;
                state_ns     = ALUWB;
            end

            AUIPC: begin
                reg_write     = 1;
                aluop         = 2'b00;
                alu_src_a     = 2'b00;
                alu_src_b     = 2'b01;
                is_immediate  = 1;
                state_ns      = FETCH;
            end

            LUI: begin
                reg_write     = 1;
                aluop         = 2'b00;
                alu_src_a     = 2'b00;
                alu_src_b     = 2'b01;
                is_immediate  = 1;
                state_ns      = FETCH;
            end

            default: state_ns = FETCH;
        endcase
    end
 
	
endmodule

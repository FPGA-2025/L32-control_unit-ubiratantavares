module Control_Unit (
    input wire clk,
    input wire rst_n,
    
    input wire [6:0] instruction_opcode, // instruction
    
    output reg pc_write,          // PCWrite
    output reg ir_write,          // IRWrite
    output reg pc_source,         // PCSource
    output reg reg_write,         // RegWrite
    output reg memory_read,       // MemRead
    output reg is_immediate,      // IsImmediate
    output reg memory_write,      // MemWrite
    output reg pc_write_cond,     // PCWriteCond
    output reg lorD,              // LordD
    output reg memory_to_reg,     // MemtoReg
    output reg [1:0] aluop,       // ALUOp (ALU_CO)
    output reg [1:0] alu_src_a,   // ALUSrcA
    output reg [1:0] alu_src_b    // ALUSrcB
);

	// estados da máquina
	localparam FETCH    = 4'b0000;  // Instruction Fetch
	localparam DECODE   = 4'b0001;  // Instruction Decode
	localparam MEMADR   = 4'b0010;  // Memory Address Computation
	localparam MEMREAD  = 4'b0011;  // Memory Read
	localparam MEMWB    = 4'b0100;  // Memory WB
	localparam MEMWRITE = 4'b0101;  // Memory Write
	localparam EXECUTER = 4'b0110;  // Execute R Type
	localparam ALUWB    = 4'b0111;  // ALUWB
	localparam EXECUTEI = 4'b1000;  // Execute I Type
	localparam JAL      = 4'b1001;  // JAL
	localparam BRANCH   = 4'b1010;  // Branch
	localparam JALR     = 4'b1011;  // JALR
	localparam AUIPC    = 4'b1100;  // AUIPC
	localparam LUI      = 4'b1101;  // LUI
	localparam JALR_PC  = 4'b1110;  // JALR PC

	// códigos de operação de instrução
	localparam LW      = 7'b0000011;
	localparam SW      = 7'b0100011;
	localparam RTYPE   = 7'b0110011;
	localparam ITYPE   = 7'b0010011;
	localparam JALI    = 7'b1101111;
	localparam BRANCHI = 7'b1100011;
	localparam JALRI   = 7'b1100111;
	localparam AUIPCI  = 7'b0010111;
	localparam LUII    = 7'b0110111;

	reg[3:0] state, next_state;

	// estado sequencial
	always @(posedge clk or negedge rst_n) begin
		if (!rst_n)
			state <= FETCH;
		else
			state <= next_state;
	end

    // transição de estados
    always @(*) begin
        case (state)
        
            FETCH:    next_state = DECODE;
            
            DECODE: begin
				case (instruction_opcode)
				   RTYPE:   next_state = EXECUTER;
				   ITYPE:   next_state = EXECUTEI;
				   JALI	:   next_state = JAL;
				   BRANCHI: next_state = BRANCH;
				   JALRI:   next_state = JALR_PC;
				   AUIPCI:  next_state = AUIPC;
				   LUII:    next_state = LUI;
				   default: next_state = MEMADR;
				endcase
            end

            MEMADR:   next_state = (instruction_opcode == LW) ? MEMREAD : MEMWRITE;

            MEMREAD:  next_state = MEMWB;

            MEMWB:    next_state = FETCH;

            MEMWRITE: next_state = FETCH;

            EXECUTER: next_state = ALUWB;

            EXECUTEI: next_state = ALUWB;

            JAL:      next_state = ALUWB;
            
			BRANCH:   next_state = FETCH;

			JALR_PC:  next_state = JALR;

			JALR:     next_state = ALUWB;

			AUIPC:    next_state = ALUWB;

			LUI:      next_state = ALUWB;
			
            ALUWB:    next_state = FETCH;

            default:  next_state = FETCH;
            
        endcase
    end

    // Geração de sinais de controle
    always @(*) begin
        // resetar todos os sinais
    	pc_write              = 0;          
    	ir_write              = 0;
        pc_source             = 0;
        reg_write             = 0;    
        memory_read           = 0;
        is_immediate          = 0;
        memory_write          = 0;
        pc_write_cond         = 0;
        lorD                  = 0;
        memory_to_reg         = 0;
        aluop                 = 2'b00;
        alu_src_a             = 2'b00;
        alu_src_b             = 2'b00;

        case (state)
            DECODE: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end

            MEMADR: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end

            MEMREAD: begin
                memory_read   = 1;
                lorD          = 1;
            end

            MEMWB: begin
                reg_write     = 1;
                memory_to_reg = 2'b01;
            end

            MEMWRITE: begin
                memory_write  = 1;
                lorD          = 1;
            end

            EXECUTER: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b00;
                aluop         = 2'b10;
            end

            ALUWB: begin
                reg_write     = 1;
                memory_to_reg = 2'b00;
            end

            EXECUTEI: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b10;
                aluop         = 2'b10;
                is_immediate  = 1;

            end

            JAL: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b01;
                pc_write      = 1;
                pc_source     = 1;
                aluop         = 2'b00;

            end

            BRANCH: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b00;
                aluop         = 2'b01;
                pc_write_cond = 1;
                pc_source     = 1;
            end

            JALR: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b01;
                pc_write      = 1;
                pc_source     = 1; 
                aluop         = 2'b00;
                is_immediate  = 1;
            end

            JALR_PC: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end

            AUIPC: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;

            end

            LUI: begin
                alu_src_a     = 2'b11;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;

            end

			FETCH: begin
		        memory_read   = 1;          
				alu_src_a     = 2'b00;
				lorD          = 0;
				ir_write      = 1;
				alu_src_b     = 2'b01;
				aluop         = 2'b00;
				pc_write      = 1;       
				pc_source     = 0;
            end

        endcase

	end
	
endmodule

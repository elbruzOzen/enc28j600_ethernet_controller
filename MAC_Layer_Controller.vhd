------------------------------------  MAC LAYER CONTROLLER --------------------------------------
-- MAC Layer is contained within ENC28J60. This module is the top control module for this device
--	It makes necessary configuration for generating MAC packets. And provides necessary information
-- and control to ENC28J60. 
--
--	Top module should write EtherType/Length(2 Byte) and Data Payload and 
-- use send to transmit MAC packet. Top module can implement IP and UDP inside data payload.
-- 
-- Wait for interrupt signal become low (means packet exists) then read data
-- (inside Ethernet Type/ Length + Data Payload ther must be length expression and place depends on 
-- protocol) When reading is finished assert rising edge to nxt_pck. If interrupt is low again continue reading
-- When it becomes high, wait until new packet comes. (When packet come, interrupt will be low again)
-- !!! Interrupt does not dealt inside this module, if control logic is needed you may use a top module.
-- !!! (INT pin of ENC28J60)

-- WR,RD,SEND,RST are rising edge sensitive. Apply rising edge, busy will become high and low 
-- then apply second command ...
-- Source and Remote MAC addresses are configurable as generics.
-------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.all;

entity MAC_Layer_Controller is
	 Generic(
			mac_src : STD_LOGIC_VECTOR(1 to 48) := (x"000000000000");
			mac_dst : STD_LOGIC_VECTOR(1 to 48) := (x"000000000000")
	 );
    Port ( clk  	 : in  STD_LOGIC;
			  miso 	 : in STD_LOGIC;
			  mosi 	 : out STD_LOGIC;
			  sck  	 : out STD_LOGIC;
			  ss 	 	 : out STD_LOGIC;
			  dout 	 : out STD_LOGIC_VECTOR(7 downto 0);
			  din  	 : in STD_LOGIC_VECTOR(7 downto 0);
			  rd 	 	 : in STD_LOGIC;
			  wr   	 : in STD_LOGIC;
			  nxt_pck : in STD_LOGIC;
			  rst  	 : in STD_LOGIC;
			  send 	 : in STD_LOGIC;
			  busy 	 : out STD_LOGIC
			  );
end MAC_Layer_Controller;

architecture Behavioral of MAC_Layer_Controller is
	
	COMPONENT Instruction_Generator
	PORT(
		opc : IN std_logic_vector(2 downto 0);
		arg : IN std_logic_vector(4 downto 0);
		din : IN std_logic_vector(7 downto 0);
		ena : IN std_logic;
		clk : IN std_logic;
		miso : IN std_logic;         
		dout : OUT std_logic_vector(7 downto 0);
		busy : OUT std_logic;
		mosi : OUT std_logic;
		ss : OUT std_logic;
		sck : OUT std_logic
		);
	END COMPONENT;
	
	-- User defined types
	type MACHINE_STATE is (STATE_INIT, STATE_IDLE, STATE_RST , STATE_SEND , STATE_RD, STATE_WR, STATE_NEXT_PCK);
	
	-- Opcode Table
	constant RCR : STD_LOGIC_VECTOR(2 downto 0) := "000";
	constant RBM : STD_LOGIC_VECTOR(2 downto 0) := "001";
	constant WCR : STD_LOGIC_VECTOR(2 downto 0) := "010";
	constant WBM : STD_LOGIC_VECTOR(2 downto 0) := "011";
	constant BFS : STD_LOGIC_VECTOR(2 downto 0) := "100";
	constant BFC : STD_LOGIC_VECTOR(2 downto 0) := "101";
	constant SC  : STD_LOGIC_VECTOR(2 downto 0) := "111";
	
	-- Input & Output Signals
	signal dout_s    : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal din_s     : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal rd_s      : STD_LOGIC := '0';
	signal wr_s      : STD_LOGIC := '0';
	signal rst_s     : STD_LOGIC := '0';
	signal send_s    : STD_LOGIC := '0';
	signal nxt_pck_s  : STD_LOGIC := '0';
	signal busy_s	  : STD_LOGIC := '1';
	
	--For edge control
	signal rd_p      : STD_LOGIC := '0';
	signal wr_p      : STD_LOGIC := '0';
	signal rst_p     : STD_LOGIC := '0';
	signal send_p    : STD_LOGIC := '0';
	signal nxt_pck_p  : STD_LOGIC := '0';
	
	-- Control signals for sub module
	signal opc_c : STD_LOGIC_VECTOR(2 downto 0) := (others => '0');
	signal arg_c : STD_LOGIC_VECTOR(4 downto 0) := (others => '0');
	signal din_c : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal dout_c : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal ena_c : STD_LOGIC := '0';
	signal busy_c : STD_LOGIC := '0';
	
	-- State variables
	signal state : MACHINE_STATE := STATE_INIT;
	signal step  : integer range 1 to 24 := 1;
	signal f_step : integer range 1 to 7 := 1;
	
	-- Keep count of package size to
	signal pack_size_cnt : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	-- For each transmit write control byte first
	signal isPreDataWritten : boolean := false;
	-- For each packet reading skip some parts
	signal isPreDataSkipped : boolean := false;
	-- Pointer of next packet
	signal next_pack_pnt : STD_LOGIC_VECTOR(15 downto 0) := "0000010111111010";	-- Start address of RX Buffer
	

begin

	Instruction_Generator_Instance: Instruction_Generator PORT MAP(
		opc => opc_c,
		arg => arg_c,
		din => din_c,
		dout => dout_c,
		busy => busy_c,
		ena => ena_c,
		clk => clk,
		miso => miso,
		mosi => mosi,
		ss => ss,
		sck => sck
	);
	
	process(clk)
	
	-- Procedures & Functions
	procedure writeContReg(constant arg: STD_LOGIC_VECTOR(4 downto 0); constant data: STD_LOGIC_VECTOR(7 downto 0)) is
	begin
		if f_step = 1 then
			opc_c <= WCR;											-- Put input of function
			arg_c <= arg;
			din_c <= data;
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then									-- IF inputs are latched, disable SPI
				ena_c <= '0';
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- Increment step of FSM
			end if;
		end if;
	end writeContReg;
	
	procedure setBitField(constant arg: STD_LOGIC_VECTOR(4 downto 0); constant data: STD_LOGIC_VECTOR(7 downto 0)) is
	begin
		if f_step = 1 then
			opc_c <= BFS;											-- Put input of function
			arg_c <= arg;
			din_c <= data;
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then									-- IF inputs are latched, disable SPI
				ena_c <= '0';
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- Increment step of FSM
			end if;
		end if;
	end setBitField;
	
	procedure clrBitField(constant arg: STD_LOGIC_VECTOR(4 downto 0); constant data: STD_LOGIC_VECTOR(7 downto 0)) is
	begin
		if f_step = 1 then
			opc_c <= BFC;											-- Put input of function
			arg_c <= arg;
			din_c <= not data;									
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then									-- IF inputs are latched, disable SPI
				ena_c <= '0';
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- Increment step of FSM
			end if;
		end if;
	end clrBitField;
	
	procedure setBank(constant bank: STD_LOGIC_VECTOR(1 downto 0)) is
	begin
		if f_step = 1 then
			if bank(0) = '0' then
				opc_c <= BFC;
				din_c <= "00000001";								-- Clear ECON1.0
			else
				opc_c <= BFS;
				din_c <= "00000001";								-- Set ECON1.0
			end if;
			arg_c <= "11111";										-- ECON1 (1F)
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then
				ena_c <= '0';										-- Disable SPI
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then									-- Wait end of operation
				if bank(1) = '0' then
					opc_c <= BFC;
					din_c <= "00000010";							-- Clear ECON1.1
				else
					opc_c <= BFS;
					din_c <= "00000010";							-- Set ECON1.1
				end if;
				f_step <= 5;
			end if;
		elsif f_step = 5 then									-- Enable SPI
			ena_c <= '1';
			f_step <= 6;
		elsif f_step = 6 then
			if busy_c = '1' then
				ena_c <= '0';
				f_step <= 7;
			end if;
		elsif f_step = 7 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;
			end if;	
		end if;
	end procedure;
	
	procedure readContReg(constant arg: STD_LOGIC_VECTOR(4 downto 0)) is
	begin
		
		if f_step = 1 then
			opc_c <= RCR;											-- Put argument and opcode
			arg_c <= arg;
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then
				ena_c <= '0';										-- Disable SPI
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- FSM next step
			end if;
		end if;
		-- This procedure is called 3 times and returns a value in dout_c
	end readContReg;
	
	procedure writeBufferMem(constant data: STD_LOGIC_VECTOR(7 downto 0)) is
	begin
		if f_step = 1 then
			opc_c <= WBM;											-- Put opcode
			din_c <= data;											-- Put data to write
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then
				ena_c <= '0';										-- Disable SPI
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- FSM next step
			end if;
		end if;
	end procedure;
	
	procedure readBufferMem is
	begin
		if f_step = 1 then
			opc_c <= RBM;											-- Put opcode
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then
				ena_c <= '0';										-- Disable SPI
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- FSM next step
			end if;
		end if;
		-- This procedure is called 3 times and returns a value in dout_c
	end readBufferMem;
	
	procedure resetModule is 
	begin 
		if f_step = 1 then
			opc_c <= SC;											-- Put reset opcode
			f_step <= 2;
		elsif f_step = 2 then
			ena_c <= '1';											-- Enable SPI
			f_step <= 3;
		elsif f_step = 3 then
			if busy_c = '1' then
				ena_c <= '0';										-- Disable SPI									
				f_step <= 4;
			end if;
		elsif f_step = 4 then
			if busy_c = '0' then
				f_step <= 1;
				step <= step + 1;									-- FSM next step
			end if;
		end if;
	end resetModule;
	
	begin
		
		if rising_edge(clk) then
		
			-- Sample inputs
			din_s <= din;
			rd_s <= rd;
			wr_s <= wr;
			rst_s <= rst;
			send_s <= send;
			nxt_pck_s <= nxt_pck;
			
			-- FSM goes here
			case state is
			
				when STATE_INIT =>
					if step = 1 then
						readContReg("11101");												-- Read ESTAT (1D)
					elsif step = 2 then
						if dout_c(0) = '1' then											   -- If ESTAT.CLK_RDY = '1' then continue
							step <= 3;
						else
							step <= 1;
						end if;
					elsif step = 3 then
						setBank("10");													  		-- Go to bank 2 for MACON2	
					elsif step = 4 then
						writeContReg("00001","00000000");								-- Clr MACON2.7(MARST)
					elsif step = 5 then
						writeContReg("00000","00001101");								-- Set some reg. MACON1(TXPAUS,RXPAUS,MARXEN)
					elsif step = 6 then
						writeContReg("00010","10110001");								-- Padding(3) + TXCRC + Full Duplex
					elsif step = 7 then
						writeContReg("00100","00010101");								-- MABBIPG(04H) is 15H for full duplex
					elsif step = 8 then
						writeContReg("00110","00010010");								-- MAIPGL(06H) is 12H
					elsif step = 9 then
						setBank("11");															-- Go to Bank 3
					elsif step = 10 then														
						writeContReg("00100",mac_src(1 to 8));							-- Write MAC Adress
				   elsif step = 11 then												
						writeContReg("00101",mac_src(9 to 16));
					elsif step = 12 then
						writeContReg("00010",mac_src(17 to 24));
					elsif step = 13 then
						writeContReg("00011",mac_src(25 to 32));
					elsif step = 14 then
						writeContReg("00000",mac_src(33 to 40));
					elsif step = 15 then
						writeContReg("00001",mac_src(41 to 48));
					elsif step = 16 then
						setBank("01");															-- Go to bank 1
					elsif step = 17 then
						writeContReg("11000","10100000");								----- FILTER CONFIGURATION -----
					elsif step = 18 then
						setBitField("11111","00000100");								 	-- Set ECON1.RXEN for enabling receptions
					elsif step = 19 then
						setBank("00");															-- Go to Bank 0
					elsif step = 20 then
						writeContReg("01000","11111010");								-- Write ERXSTL
					elsif step = 21 then
						writeContReg("01001","00000101");								-- Write ERXSTH
					elsif step = 22 then
						writeContReg("11011","11000000");								-- Set EIE.PKTIE ans EIE.INTIE for enabling int.
					elsif step = 23 then
						step <= 1;
						state <= STATE_IDLE;
					end if;
				when STATE_IDLE =>
					if rd_s = '1' and rd_p = '0' then									-- Detect if there is any rising edge
						state <= STATE_RD;
						busy_s <= '1';
					elsif wr_s = '1' and wr_p = '0' then
						state <= STATE_WR;
						busy_s <= '1';
					elsif rst_s = '1' and rst_p = '0' then
						state <= STATE_RST;
						busy_s <= '1';
					elsif send_s = '1' and send_p = '0' then
						state <= STATE_SEND;
						busy_s <= '1';
					elsif nxt_pck_s = '1' and nxt_pck_p = '0' then
						state <= STATE_NEXT_PCK;
						busy_s <= '1';
					else
						busy_s <= '0';															-- Machine is in IDLE state
					end if;
				when STATE_RST  =>
					if step = 1 then
						resetModule;															-- Send reset command
					elsif step = 2 then
						step <= 1;
						state <= STATE_INIT;
					end if;
				when STATE_SEND =>
					if step = 1 then
						setBank("00");															-- Go to bank '0'
					elsif step = 2 then
						writeContReg("00110",pack_size_cnt(7 downto 0)-1);			-- Write ETXND Low Byte
					elsif step = 3 then
						writeContReg("00111",pack_size_cnt(15 downto 8));			-- Write ETXND High Byte
					elsif step = 4 then
						setBitField("11111","00001000");									-- Set ECON1.TXRTS
					elsif step = 5 then
						step <= 1;
						pack_size_cnt <= (others => '0');								-- Reset Package Counter
						isPreDataWritten <= false;											-- New control byte + MAC addresses is needed for next frame
						state <= STATE_IDLE;													-- Return to idle state
					end if;
				when STATE_RD  =>
					if step = 1 then
						if isPreDataSkipped then											-- If we start new packet, we need to skip parts
							step <= 23;
						else
							step <= 2;
						end if;
					elsif step = 2 then
						readBufferMem;
					elsif step = 3 then
						next_pack_pnt(7 downto 0) <= dout_c;							-- Save next packet adress low byte
						step <= 4;
					elsif step = 4 then
						readBufferMem;
					elsif step = 5 then
						next_pack_pnt(15 downto 8) <= dout_c;							-- Save next packet adress high byte
						step <= 6;
					elsif step = 6 then														-- Skip status vector
						readBufferMem;															
					elsif step = 7 then
						readBufferMem;
					elsif step = 8 then
						readBufferMem;
					elsif step = 9 then
						readBufferMem;
					elsif step = 10 then														-- Skip destination MAC
						readBufferMem;
					elsif step = 11 then
						readBufferMem;
					elsif step = 12 then														
						readBufferMem;
					elsif step = 13 then
						readBufferMem;
					elsif step = 14 then
						readBufferMem;
					elsif step = 15 then
						readBufferMem;
					elsif step = 16 then													   -- Skip source MAC
						readBufferMem;	
					elsif step = 17 then
						readBufferMem;
					elsif step = 18 then
						readBufferMem;
					elsif step = 19 then
						readBufferMem;
					elsif step = 20 then
						readBufferMem;
					elsif step = 21 then
						readBufferMem;
					elsif step = 22 then
						isPreDataSkipped <= true;											-- We skipped address part for this packet
						step <= 23;
					elsif step = 23 then														-- Read actual data (After addresses)
						readBufferMem;
					elsif step = 24 then
						dout_s <= dout_c;														-- Put read data to output
						step <= 1;
						state <= STATE_IDLE;													-- Go to idle state
					end if;
				when STATE_NEXT_PCK =>
					if step = 1 then
						setBank("00");															-- Set bank
					elsif step = 2 then
						writeContReg("00000",next_pack_pnt(7 downto 0));			-- Set ERDPTL 
					elsif step = 3 then
						writeContReg("00001",next_pack_pnt(15 downto 8));			-- Set ERDPTH
					elsif step = 4 then
						writeContReg("01100",next_pack_pnt(7 downto 0));			-- Set ERXRDPTL for freeing space
					elsif step = 5 then
						writeContReg("01101",next_pack_pnt(15 downto 8));			-- Set ERXRDPTH for freeing space
					elsif step = 6 then
						setBitField("11110","01000000");									-- Set ECON2.PKTDEC for decrement package count
					elsif step = 7 then
						isPreDataSkipped <= false;											-- For new packet we need to skip adress again
						step <= 1;																
						state <= STATE_IDLE;													-- Return to idle state
					end if;
				when STATE_WR  =>
				 	if step = 1 then
						if (isPreDataWritten = false) then								-- For each packet write control byte first
							step <= 2;
						else
							step <= 21;
						end if;									
					elsif step = 2 then
						setBank("00");															-- Go to Bank 0
					elsif step = 3 then
						writeContReg("00010","00000000");								-- Clear EWRPTL
					elsif step = 4 then
						writeContReg("00011","00000000");								-- Clear EWRPTH
					elsif step = 5 then
						writeContReg("00100","00000000");								-- Clear ETXSTL
					elsif step = 6 then
						writeContReg("00101","00000000");								-- Clear ETXSTH
					elsif step = 7 then
						writeBufferMem("00001110");										-- Write Control Byte
					elsif step = 8 then
						writeBufferMem(mac_dst(1 to 8));									-- Write destination MAC address
					elsif step = 9 then
						writeBufferMem(mac_dst(9 to 16));
					elsif step = 10 then
						writeBufferMem(mac_dst(17 to 24));
					elsif step = 11 then
						writeBufferMem(mac_dst(25 to 32));
					elsif step = 12 then
						writeBufferMem(mac_dst(33 to 40));
					elsif step = 13 then
						writeBufferMem(mac_dst(41 to 48));
					elsif step = 14 then
						writeBufferMem(mac_src(1 to 8));										-- Write source MAC address
					elsif step = 15 then
						writeBufferMem(mac_src(9 to 16));
					elsif step = 16 then
						writeBufferMem(mac_src(17 to 24));
					elsif step = 17 then
						writeBufferMem(mac_src(25 to 32));
					elsif step = 18 then
						writeBufferMem(mac_src(33 to 40));
					elsif step = 19 then
						writeBufferMem(mac_src(41 to 48));
					elsif step = 20 then
						isPreDataWritten <= true;
						pack_size_cnt <= pack_size_cnt + 13;						   -- Keep size of written package
						step <= 21;
					elsif step = 21 then
						writeBufferMem(din_s);												-- Write data package
					elsif step = 22 then
						pack_size_cnt <= pack_size_cnt + 1;								-- Keep size of written package
						step <= 1;
						state <= STATE_IDLE;
					end if;
			end case;
			
			-- Save previous values
			rd_p <= rd_s;
			wr_p <= wr_s;
			rst_p <= rst_s;
			send_p <= send_s;
			nxt_pck_p <= nxt_pck_s;
			
			-- Assert outputs
			dout <= dout_s;
			--pck_exst <= pck_exst_s;
			busy <= busy_s;
		
		end if;
		
	end process;
end Behavioral;
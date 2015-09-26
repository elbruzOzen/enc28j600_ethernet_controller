---------------------------- ENC28J60 SPI INSTRUCTION GENERATOR -------------------------------------
-- OPERATION     OPCODE     ARGUMENT      DATAIN     DATAOUT    EXPLANATION 
-- RCR 				000         ADDR 			   X         DATA 	 ENA = 1 -> WAIT BUSY R.E -> ENA = 0
-- RBM				001 			 X 				X 			 DATA	    ENA = 1 -> WAIT BUSY R.E -> ENA = 0 (*)
--	WCR 				010			ADDR 			  DATA        X 		 ENA = 1 -> WAIT BUSY R.E -> ENA = 0	
-- BFS 				100 			ADDR 			  DATA        X 		 ENA = 1 -> WAIT BUSY R.E -> ENA = 0
-- BFC 				101  			ADDR 			  DATA        X 		 ENA = 1 -> WAIT BUSY R.E -> ENA = 0
--	SC 				111 			 X             X          X       ENA = 1 -> WAIT BUSY R.E -> ENA = 0	 
-- WBM 				011			 X            DATA	 	  X 		 ENA = 1 -> WAIT BUSY R.E -> ENA = 0 (*)

-- (*) Multiple read/write is supported without opc+arg overhead. For this, keep ENA high, read output
-- or write new input after each rising edge of busy. Make ENA low after last write / Make ENA low and 
-- read last input after rising edge.
-----------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Instruction_Generator is
    Port ( opc : in  STD_LOGIC_VECTOR (2 downto 0);
           arg : in  STD_LOGIC_VECTOR (4 downto 0);
           din : in  STD_LOGIC_VECTOR (7 downto 0);
           dout : out  STD_LOGIC_VECTOR (7 downto 0);
           busy : out  STD_LOGIC;
           ena : in  STD_LOGIC;
			  clk : in STD_LOGIC;
			  miso : in STD_LOGIC;
			  mosi : out STD_LOGIC;
			  ss : out STD_LOGIC;
			  sck : out STD_LOGIC);
end Instruction_Generator;

architecture Behavioral of Instruction_Generator is

	COMPONENT SPI_Master
	PORT(
		din : IN std_logic_vector(7 downto 0);
		ena : IN std_logic;
		rw : IN std_logic;
		miso : IN std_logic;
		clk : IN std_logic;          
		dout : OUT std_logic_vector(7 downto 0);
		busy : OUT std_logic;
		mosi : OUT std_logic;
		ss : OUT std_logic;
		sck : OUT std_logic
		);
	END COMPONENT;
	
	--User defined types
	type MACHINE_INSTRUCTION is (IDLE, RCR, RBM , WCR , WBM, SC); -- BFS, BFC are same with WCR
	
	-- Outside sample & asserted signals
	signal opc_s : STD_LOGIC_VECTOR (2 downto 0) := (others => '0');
	signal arg_s : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
	signal din_s : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal dout_s : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal busy_s : STD_LOGIC := '0';
	signal ena_s : STD_LOGIC := '0';
	
	-- SPI Master control signals
	signal din_c : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal dout_c : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
	signal busy_c : STD_LOGIC := '0';
	signal ena_c : STD_LOGIC := '0';
	signal rw_c : STD_LOGIC := '0';
	
	-- Input Latch & Output Buffer signals
	signal arg_b : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
	signal din_b : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');

	-- State variables for FSM
	signal instruction : MACHINE_INSTRUCTION := IDLE;
	signal step : integer range 1 to 15 := 1;
	
begin
	
	-- Port Map for SPI Master
	SPI_Master_Instance: SPI_Master PORT MAP(
		din => din_c,
		dout => dout_c,
		ena => ena_c,
		rw => rw_c,
		busy => busy_c,
		mosi => mosi,
		miso => miso,
		ss => ss,
		sck => sck,
		clk => clk
	);

	process(clk)
	begin
		if rising_edge(clk) then
			
			-- Sample inputs
			opc_s <= opc;
			arg_s <= arg;
			din_s <= din;
			ena_s <= ena;
			
			-- FSM logic
			case instruction is
				when IDLE =>
					if ena_s = '1' then
						arg_b <= arg_s;
						busy_s <= '1';
						din_b <= din_s;
						case opc_s is
							when "000" => instruction <= RCR;
							when "001" => instruction <= RBM;
							when "010" => instruction <= WCR;
							when "011" => instruction <= WBM;
							when "100" => instruction <= WCR;	--BFS 
							when "101" => instruction <= WCR;	--BFC
							when "111" => instruction <= SC;
							when others => instruction <= IDLE;
						end case;
					else
						busy_s <= '0';
  					end if;
				when RCR =>
					if step = 1 then
						din_c <= opc_s & arg_s;														-- Opcode + Argument(Addr.)													
						rw_c <= '0';																	-- Write
						step <= 2;
					elsif step = 2 then
						ena_c <= '1';																	-- Enable SPI
						step <= 3;
					elsif step = 3 then
						if busy_c = '1' then															-- Write operation is latched
							rw_c <= '1';																-- Next read operation
							step <= 4;																
						end if;
					elsif step = 4 then
						if busy_c = '0' then															-- First read finished
							step <= 5;
						end if;
					elsif step = 5 then
						if busy_c = '1' then															-- Real read started
							ena_c <= '0';																-- Disable SPI
							step <= 6;
						end if;
					elsif step = 6 then
						if busy_c = '0' then															-- Wait until end of transmission
							dout_s <= dout_c;															-- Get output
							step <= 1;
							instruction <= IDLE;														-- Return to idle state
						end if;
					end if;
				when RBM =>
					if step = 1 then
						din_c <= opc_s & "11010";													-- Send command
						rw_c <= '0';																	-- Write
						step <= 2;
					elsif step = 2 then
						ena_c <= '1';																	-- Enable SPI
						step <= 3;
					elsif step = 3 then	
						if busy_c = '1' then															-- If writing operation is latched
							rw_c <= '1';																-- Put reading				
							step <= 4;
						end if;
					elsif step = 4 then
						if busy_c = '0' then
							busy_s <= '1';
							step <= 5;
						end if;
					elsif step = 5 then
						if busy_c = '1' then
							if ena_s = '0' then
								ena_c <= '0';
							else
								ena_c <= '1';
							end if;
							step <= 6;
						end if;
					elsif step = 6 then
						if busy_c = '0' then
							dout_s <= dout_c;
							busy_s <= '0';
							if ena_s = '0' then
								step <= 1;
								instruction <= IDLE;
							else
								step <= 4;
							end if;
						end if;
					end if;
				when WCR =>
					if step = 1 then
						din_c <= opc_s & arg_b;														-- Opcode + Argument
						rw_c <= '0';																	-- Write
						step <= 2;
					elsif step = 2 then
						ena_c <= '1';																	--	Enable
						step <= 3;
					elsif step = 3 then
						if busy_c = '1' then
							din_c <= din_s;															-- Put another input
							step <= 4;
						end if;
					elsif step = 4 then
						if busy_c = '0' then
							step <= 5;
						end if;
					elsif step = 5 then
						if busy_c = '1' then
							ena_c <= '0';																-- If all input latched
							step <= 6;																	-- Disable chip
						end if;
					elsif step = 6 then
						if busy_c = '0' then															-- Wait for end of operation
							step <= 1;
							instruction <= IDLE;														-- Return to idle state
						end if;
					end if;
				when WBM =>
					if step = 1 then
						din_c <= opc_s & "11010";													-- Put input into SPI
						rw_c <= '0';
						step <= 2;
					elsif step = 2 then
						ena_c <= '1';																	-- Enable SPI
						step <= 3;
					elsif step = 3 then
						if busy_c = '1' then
							din_c <= din_b;															-- Put new input
							step <= 4;
						end if;
					elsif step = 4 then
						if busy_c = '0' then
							busy_s <= '0';																-- Input is latched
							step <= 5;
						end if;
					elsif step = 5 then
						if busy_c = '1' then
							if ena_s = '1' then
								ena_c <= '1';
								din_c <= din_s;														-- Continue operation without 
								busy_s <= '1';															-- Pulling ss high
								step <= 4;
							else
								ena_c <= '0';															-- Terminate operation
								step <= 6;
							end if;														
						end if;
					elsif step = 6 then
						if busy_c = '0' then														  -- Wait end of transmission
							step <= 1;	
							instruction <= IDLE;													  -- Return to initial state
						end if;
					end if;
				when SC =>
					if step = 1 then
						din_c <= "11111111";  														-- Reset argument
						rw_c <= '0';																	-- Write		
						step <= 2;
 					elsif step = 2 then
						ena_c <= '1';																	-- Enable
						step <= 3;
					elsif step = 3 then
						if busy_c = '1' then															-- Wait until input is latched
							ena_c <= '0';																-- Disable SPI
							step <= 4;
						end if;
					elsif step = 4 then
						if busy_c = '0' then															-- Wait until transmission finished
							step <= 1;
							instruction <= IDLE;														-- Return to idle state
						end if;
					end if;
			end case;
			
			-- Assert outputs
			dout <= dout_s;
			busy <= busy_s;
			
		end if;
	end process;
	
end Behavioral;
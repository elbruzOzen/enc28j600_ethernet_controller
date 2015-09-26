---------------------------------- SPI MASTER CONTROLLER --------------------------------------------------
-- Two basic operation is defined. Byte write and byte read. Byte read does not send any instruction before.
-- It is user's responsibility to combined write or read bytes for more complex functionality.
-- Provide RW:0 and din for write operation. RW: 1 for read operation
-- When ena asserted high, ss goes low and operation starts. If enable is high at the end of operation 
-- next operation is performed otherwise ss goes high and machine turns into idle state.
-----------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity SPI_Master is
    Generic( clk_freq : integer := 100000000;					-- 100 MHz
				 bus_freq : integer := 10000000);					-- 10  MHz
    Port ( din : in  STD_LOGIC_VECTOR (7 downto 0);
           dout : out  STD_LOGIC_VECTOR (7 downto 0);
           ena : in  STD_LOGIC;
           rw : in  STD_LOGIC;
           busy : out  STD_LOGIC;
           mosi : out  STD_LOGIC;
           miso : in  STD_LOGIC;
           ss : out  STD_LOGIC;
           sck : out  STD_LOGIC;
           clk : in  STD_LOGIC);
end SPI_Master;

architecture Behavioral of SPI_Master is
	
	--User defined types
	type MACHINE_STATE is (ready, start , send , receive, wait_hold_time);
	
	-- Constants
	constant F_DIV_COUNTER_SIZE : integer := (clk_freq/bus_freq)/2;
	
	--Frequncy divider
	signal f_div_counter : integer range 1 to F_DIV_COUNTER_SIZE := 1;
	signal f_div_enable  : STD_LOGIC := '0';
	
	-- IO signals
	signal din_s : STD_LOGIC_VECTOR(7 downto 0) := (others =>'0');
	signal dout_s : STD_LOGIC_VECTOR(7 downto 0) := (others =>'0');
	signal ena_s : STD_LOGIC := '0';
	signal rw_s : STD_LOGIC := '0';
	signal busy_s : STD_LOGIC := '0';
	signal mosi_s : STD_LOGIC := '0';
	signal miso_s : STD_LOGIC := '0';
	signal ss_s : STD_LOGIC := '0';
	signal sck_s : STD_LOGIC := '0';
	
	-- Buffers to play in FSM
	signal din_b : STD_LOGIC_VECTOR(7 downto 0) := (others =>'0');
	signal dout_b : STD_LOGIC_VECTOR(7 downto 0) := (others =>'0');
	
	--State
	signal state : MACHINE_STATE := ready;
	signal bit_cnt : integer range 0 to 8 := 8;
	signal substate : integer range 1 to 9 := 1;
	
begin
	process(clk)
	begin
		if rising_edge(clk) then
			
			-- Sample inputs
			din_s <= din;
			ena_s <= ena;
			rw_s <= rw;
			miso_s <= miso;
			
			-- Divide frequncy for FSM
			if f_div_counter = F_DIV_COUNTER_SIZE then
				f_div_counter <= 1;
				f_div_enable <= '1';
			else
				f_div_counter <= f_div_counter + 1;
				f_div_enable <= '0';
			end if;
			
			-- FSM goes here
			if f_div_enable = '1' then
				
				case state is 
					when ready =>
						if substate = 1 then
							ss_s <= '1';
							substate <= 2;
						elsif substate = 2 then
							if ena_s = '1' then										-- If enable is high
								state <= start;										-- Begin transmission
							else
								sck_s <= '0';											-- Else wait by pulling lines to idle positions
								mosi_s <= '1';
								ss_s <= '1';
							end if;
							substate <= 1;
						end if;
					when start =>
						if substate = 1 then
							ss_s <= '0';												-- Select slave, CS setup time wait
							substate <= 2;
						elsif substate = 2 then
							din_b <= din_s;											-- Get sample for operation	
							busy_s <= '1';												-- Input is latched
							if rw_s = '0' then										-- Select operation
								state <= send;
								mosi_s <= din_s(7);									-- Put data on line, not din_b(7) first time because assignment will be complete on next tick 
								din_b <= din_s(6 downto 0) & din_s(7);			-- Put shifted version in buffer
							else
								state <= receive;
							end if;
							substate <= 1;
						end if;
					when send =>				
						if bit_cnt /= 0 then
							if sck_s = '1' then									-- On falling edges, put data
								mosi_s <= din_b(7);								-- After operation started, din_b will be used for putting data
								din_b <= din_b(6 downto 0) & din_b(7);		-- Shift after each bit is served
								sck_s <= '0';
							else
								bit_cnt <= bit_cnt - 1;							-- Decrement bit_cnt
								sck_s <= '1';										-- Rising edge means that slaves can sample data
							end if;
						else
							bit_cnt <= 8;											-- Clear bit_cnt
							busy_s <= '0';											-- Operation is finished
							sck_s <= '0';											-- Sck goes to idle state
							if ena_s = '1' then									-- Continue or not
								state <= start;
							else
								state <= wait_hold_time;
							end if;
						end if;
					when receive =>												
						if bit_cnt /= 0 then
							if sck_s = '1' then
								dout_b <= dout_b(6 downto 0) & dout_b(7); -- Shift on falling edges
								sck_s <= '0';	
							else
								dout_b(7) <= miso_s;								-- Sample data on rising edges
								bit_cnt <= bit_cnt - 1;
								sck_s <= '1';
							end if;
						else
							dout_s <= dout_b(6 downto 0) & dout_b(7);		-- Shift one more time
							bit_cnt <= 8;
							busy_s <= '0';											-- Operation is finished
							sck_s <= '0';											-- Sck goes to idle state
							if ena_s = '1' then									-- Continue or not
								state <= start;
							else
								state <= wait_hold_time;
							end if;
						end if;
					when wait_hold_time =>										-- For writing registers, an extra time needed 
						if substate = 9 then										-- before pulling ss high
							substate <= 1;
							state <= ready;
						else
							substate <= substate + 1;
						end if;
				end case;
				
			end if;
			
			-- Assert outputs
			dout <= dout_s;
			busy <= busy_s;
			mosi <= mosi_s;
			ss <= ss_s;
			sck <= sck_s;
			
		end if;
	end process;
end Behavioral;
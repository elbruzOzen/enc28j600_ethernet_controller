----------------------------------------------------------------------------------------------------------
--
-- This module connects a UDP controller with 2 FIFO Memory for transmit and receive.
-- To use with an embedded system like microcontroller, read the parts about usage
-- Default FIFO size is 1K for each. Uses 100MHz clock default, but you can configure. 
-- SPI speed for ENC28J60 is 10MHz as default and max. You can operate at lower frequnecies by 
-- setting generics in SPIMaster. Please look at the information abut sub-modules.

-- FOR READING:
-- If empty_rx is low, means that data is waiting for you. You can start reading operation.
-- Make sure busy is low. Assert ena high. This will give the control of FIFO's to you.
-- Read data by applying rising edge to rd pin. First two byte is data size. (Size bytes are not included)
-- If empty_tx does not become high, you can continue reading. Again fisrt two bytes are size.
-- After reading finished, make ena low. This will enable UDP-FIFO transimission.

-- FOR WRITING:
-- If full_tx is low, you can write a data to send. Make sure busy is low. 
-- Assert ena high. This will give the control of FIFO's to you.
-- Write data by applying rising edge to wr pin. packet structure nust be as follows:
-- IP Adress (4 Byte) + UDP Port (4Byte) + Data Size (2 Byte) + Data
-- You can write more than one packages without making ena low. When you make ena low. Data will be started
-- to fetched to send.

-- Reset is the system reset for ENC28J60. Rising edge sensitive.

-- UDP Controller does not support ARP requests, or it does not send any requests. You may need to use static 
-- ARP commands for other devices that communicates with UDP Controller. Any packets that is not UDP and IP + Port 
-- adress does not match, will be discarded.

-----------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Top_Design is
    Port ( clk      : in  STD_LOGIC;
			  din      : in  STD_LOGIC_VECTOR(7 downto 0);
			  dout     : out STD_LOGIC_VECTOR(7 downto 0);
			  rd       : in  STD_LOGIC;
			  wr       : in  STD_LOGIC;
			  miso     : in  STD_LOGIC; 	---					
			  mosi     : out STD_LOGIC; 	---
			  sck      : out STD_LOGIC; 	---	Module Connections
			  ss       : out STD_LOGIC; 	---
			  int  	  : in  STD_LOGIC; 	---
			  empty_rx : out STD_LOGIC;
			  full_tx  : out STD_LOGIC;
			  busy 	  : out STD_LOGIC;
			  ena 	  : in  STD_LOGIC;
			  rst      : in  STD_LOGIC);  ---   Directly connected to sub-module
end Top_Design;

architecture Behavioral of Top_Design is

	-- Sub-Module Declarations 
	COMPONENT IP_UDP_Controller
	PORT(
		clk : IN std_logic;
		miso : IN std_logic;
		int : IN std_logic;
		din : IN std_logic_vector(7 downto 0);
		wr : IN std_logic;
		rd : IN std_logic;
		send : IN std_logic;
		nxt_pck : IN std_logic;
		rst : IN std_logic;          
		mosi : OUT std_logic;
		sck : OUT std_logic;
		ss : OUT std_logic;
		dout : OUT std_logic_vector(7 downto 0);
		busy : OUT std_logic;
		addr_err : OUT std_logic;
		data_exst : OUT std_logic
		);
	END COMPONENT;
	
	COMPONENT FIFO_1K
	PORT(
		 clk : IN STD_LOGIC;
		 din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 wr_en : IN STD_LOGIC;
		 rd_en : IN STD_LOGIC;
		 dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		 full : OUT STD_LOGIC;
		 empty : OUT STD_LOGIC
		);
	END COMPONENT;
	
	-- Input & Output Signals
	signal din_s  		: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal dout_s 		: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal rd_s   		: STD_LOGIC := '0';
	signal wr_s   		: STD_LOGIC := '0';
	signal empty_rx_s : STD_LOGIC := '0';
	signal full_tx_s  : STD_LOGIC := '0';
	signal ena_s 		: STD_LOGIC := '0';
	signal busy_s 		: STD_LOGIC := '0';

	-- Interconnections
	signal data_udp_rx  : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal data_udp_tx  : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	
	-- UDP Module Control Signals
	signal wr_udp 			: STD_LOGIC := '0';
	signal rd_udp 			: STD_LOGIC := '0';
	signal send_udp 		: STD_LOGIC := '0';
	signal nxt_pck_udp 	: STD_LOGIC := '0';
	signal busy_udp      : STD_LOGIC := '0';
	signal addr_err_udp  : STD_LOGIC := '0';
	signal data_exst_udp : STD_LOGIC := '0';
	
	-- Receive FIFO Control Signals
	signal full_rx      : STD_LOGIC := '0';
	signal clk_input_rx : STD_LOGIC := '0';	-- Look combinational expression below
	signal wr_en_rx	  : STD_LOGIC := '0';
	signal clk_rx  	  : STD_LOGIC := '0';	-- Change this
	
	-- Transmit FIFO Control Signals
	signal empty_tx     : STD_LOGIC := '0';
	signal clk_input_tx : STD_LOGIC := '0';   -- Look combinational expression below
	signal rd_en_tx	  : STD_LOGIC := '0';
	signal clk_tx  	  : STD_LOGIC := '0';   -- Change this
	
	-- State variables
	signal state  : integer range 1 to 26 := 1;
	signal f_step : integer range 1 to  4 := 1;

	-- Size variables
	signal readSize : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	signal writeSize : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	
	-- Others
	signal byteCount : integer range 1 to 6 := 1;

begin
	
	-- Sub Module instances
	UDP_Controller: IP_UDP_Controller PORT MAP(
		clk => clk,
		miso => miso,
		mosi => mosi,
		sck => sck,
		ss => ss,
		int => int,
		din => data_udp_tx,
		dout => data_udp_rx,
		wr => wr_udp,
		rd => rd_udp,
		send => send_udp,
		nxt_pck => nxt_pck_udp,
		rst => rst,
		busy => busy_udp,
		addr_err => addr_err_udp,
		data_exst => data_exst_udp
	);

	-- One FIFO is coming data
	RX_FIFO : FIFO_1K
	PORT MAP (
	 clk => clk_input_rx,
	 din => data_udp_rx,
	 wr_en => wr_en_rx,
	 rd_en => ena_s,
	 dout => dout_s,
	 full => full_rx,
	 empty => empty_rx_s
	);

	-- Other for transmitting data
	TX_FIFO : FIFO_1K
	PORT MAP (
	 clk => clk_input_tx,
	 din => din_s,
	 wr_en => ena_s,
	 rd_en => rd_en_tx,
	 dout => data_udp_tx,
	 full => full_tx_s,
	 empty => empty_tx
	);
	
	process(clk)
	
		-- Some common functions & procedures that used again and again
		-- Write UDP
		procedure writeUDP is
		begin
			if f_step = 1 then
				wr_udp <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_udp = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_udp = '0' then
					wr_udp <= '0';
					f_step <= 1;
					state <= state + 1;
				end if;
			end if;
		end procedure;
		
		-- Read UDP
		procedure readUDP is					
		begin
			if f_step = 1 then
				rd_udp <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_udp = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_udp = '0' then
					rd_udp <= '0';
					f_step <= 1;
					state <= state + 1;
				end if;
			end if;
		end procedure;
		
		-- Send Data UDP
		procedure sendData is					
		begin
			if f_step = 1 then
				send_udp <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_udp = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_udp = '0' then
					send_udp <= '0';
					f_step <= 1;
					state <= state + 1;
				end if;
			end if;
		end procedure;
		
		-- Next packet UDP
		procedure nextPack is					
		begin
			if f_step = 1 then
				nxt_pck_udp <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_udp = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_udp = '0' then
					nxt_pck_udp <= '0';
					f_step <= 1;
					state <= state + 1;
				end if;
			end if;
		end procedure;
		
		-- Write RX FIFO
		procedure writeFIFO is
		begin
			if f_step = 1 then
				wr_en_rx <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				clk_rx <= '1';
				f_step <= 3;
			elsif f_step = 3 then
				clk_rx <= '0';
				f_step <= 4;
			elsif f_step = 4 then
				wr_en_rx <= '0';
				f_step <= 1;
				state <= state + 1;
			end if;
		end procedure;
		
		-- Read TX FIFO
		procedure readFIFO is
		begin
			if f_step = 1 then
				rd_en_tx <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				clk_tx <= '1';
				f_step <= 3;
			elsif f_step = 3 then
				clk_tx <= '0';
				f_step <= 4;
			elsif f_step = 4 then
				rd_en_tx <= '0';
				f_step <= 1;
				state <= state + 1;
			end if;
		end procedure;
		
	-- Sequential Design & FSM
	begin
	
		if rising_edge(clk) then
			
			--Sample inputs
			din_s <= din;
			rd_s <= rd;
			wr_s <= wr;
			ena_s <= ena;
			
			-- FSM
			case state is
				when 1 => -- Select which operation to perform
					if data_exst_udp = '1' and ena_s = '0' and full_rx = '0' then	-- Write coming data to FIFO
						busy_s <= '1';
						state <= 2;
					elsif empty_tx = '0' and ena_s = '0' then	-- Write data from FIFO to UDP
						busy_s <= '1';
						state <= 13;
					else
						busy_s <= '0';		-- Wait as idle
					end if;
				------------  READ  --------------------------
				-- First two byte is size, therefore write it to FIFO, also keep the number for reading remaining
				when 2 =>
					readUDP;
				when 3 =>
					if addr_err_udp = '0' then
						readSize(15 downto 8) <= data_udp_rx;
						state <= state + 1;
					else
						state <= 11;
					end if;
				when 4 =>
					writeFIFO;
				when 5 =>
					readUDP;
				when 6 =>
					readSize(7 downto 0) <= data_udp_rx;
					state <= state + 1;
				when 7 =>
					writeFIFO;
				when 8 =>
					-- Read as size number, write to FIFO
					readUDP;
				when 9 =>
					writeFIFO;
				when 10 =>
					if readSize /= 1 then
						readSize <= readSize - 1;
						state <= 8;
					else
						state <= 11;
					end if;
				when 11 =>
					-- Jump to next data packet
					nextPack;
				when 12 =>
					-- Go to initial state
					state <= 1;
				-------------  WRITE  ------------------------
				-- Write adresses 4(IP Adress) + 2(UDP Port) to UDP
				when 13 =>
					readFIFO;
				when 14 =>
					writeUDP;
				when 15 =>
					if byteCount /= 6 then
						byteCount <= byteCount + 1;
						state <= 13;
					else
						byteCount <= 1;
						state <= 16;
					end if;
				when 16 =>
					readFIFO;
				-- While writing size, also keep record of it, we will use it for writing remaining part of data
				when 17 =>
					writeSize(15 downto 8) <= data_udp_tx;
					state <= state + 1;
				when 18 =>
					writeUDP;
				when 19 =>
					readFIFO;
				when 20 =>
					writeSize(7 downto 0) <= data_udp_tx;
					state <= state + 1;
				when 21 =>
					writeUDP;
				when 22 =>
				-- Write remaining part of the data
					readFIFO;
				when 23 =>
					writeUDP;
				when 24 =>
					if writeSize /= 1 then
						writeSize <= writeSize - 1;
						state <= 22;
					else
						state <= 25;
					end if;
				when 25 =>
				-- Send command to UDP
					sendData;
				when 26 =>
				-- Return to initial state
					state <= 1;
			end case;
			
			-- Assert outputs
			dout <= dout_s;
			empty_rx <= empty_rx_s;
			full_tx <= full_tx_s;
			busy <= busy_s;
			
		end if;
	end process;

	-- You may consider to take this inside process
	clk_input_rx <= clk_rx or rd_s;
	clk_input_tx <= clk_tx or wr_s;
  
end Behavioral;
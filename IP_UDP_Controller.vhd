----------------------------  IPV4&UDP LAYER CONTROLLER ------------------------------------
--
-- This modules generates UDP packets over IPV4 protocol for given data. 
--
-- For writing, put destination IP adress (IPV4) (4 Byte) + Destination port (UDP) (2 Byte) 
-- + Data Length(2 Byte) + Data Payload. Save each byte using rising edge to WR pin.
-- When you finished writing data, apply rising edge to SEND.
-- 
-- When DATA_EXST is high, means, there is a packet. You can start reading by applying rising 
-- edge to RD. First two bytes are data length. You can continue reading by appliying rising 
-- edge to RD. After you finished reading. Apply rising edge to NXT_PCK. If DATA_EXST still 
-- high you can continue reading next packet. If not, wait until DATA_EXST become high.
-- (PS. DATA_EXST is actually inverted INT pin of ENC28J60). Src MAC, IP, and PORT are given 
-- as generic to module. As default, I have closed accepting MAC Broadcast. Probably you will 
-- need to enable BROADCAST reception to receive standard UDP packages from PC, Many higher 
-- protocols use BROADCAST option for MAC. (Ex. UDP). But, if you do not read,
-- network devices fill buffer of ENC28J60 in a short time and device become useless for reception. 
-- For top module implementation,(assuming broadcast is enabled) read when DATA_EXST is high, after
-- first read of each packet, check ADDR_ERR. if it is low, means that, IP and Port adresses 
-- are same as our adress(given as generics). The you can continue reading normally as above.
-- If ADDR_ERR  become high, you can discard this packet using NXT_PCK. If you apply NXT_PCK it will
-- turn into low until you read a new packet. (If packet adresses match it will remain low) 
-- Reset is also triggered by rising edge, it is system reset for ENC28J60.

-- !!!!! Please wait busy become low before applying next rising edge. !!!!!!!

-- How to enable MAC broadcast reception: Find this line in MAC LAYER CONTROLLER
-- writeContReg("11000","10100000");	----- FILTER CONFIGURATION -----
-- and make it: 
-- writeContReg("11000","10100001");	----- FILTER CONFIGURATION -----
-- 
-- Checksums: 
-- MAC Checksum handled by ENC28J60. 
-- UDP Checksum is optional. We skip it in reception, we write 0x0000 in  transmission.
-- IPV4 Header checksum is not optional. IP_UDP_Controller generates checksum automatically. (Do not worry :))
-- For reception, IP_UDP_Controller does not care IPV4 Header checksum. Simply skips it.
--------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity IP_UDP_Controller is
	 Generic(
			src_mac 	: STD_LOGIC_VECTOR(1 to 48) := (x"010203040506"); 		 	
			src_ip 	: STD_LOGIC_VECTOR(1 to 32) := (x"07080910");			
			src_port : STD_LOGIC_VECTOR(1 to 16) := (x"1112");					
			dst_mac 	: STD_LOGIC_VECTOR(1 to 48) := (x"FFFFFFFFFFFF")							
	 );
    Port ( clk  : in  STD_LOGIC;											
			  miso : in  STD_LOGIC;											-->
			  mosi : out STD_LOGIC;											-->
			  sck  : out STD_LOGIC;											-->	ENC28J60 Connections
			  ss   : out STD_LOGIC;											-->
			  int  : in STD_LOGIC;											-->
			  din  : in  STD_LOGIC_VECTOR(7 downto 0);
			  dout : out STD_LOGIC_VECTOR(7 downto 0);
			  wr	 : in  STD_LOGIC;
			  rd	 : in  STD_LOGIC; 										-->  
			  send : in  STD_LOGIC;											-->  Rising Edge Sensitive
			  nxt_pck : in STD_LOGIC;										-->  While Busy Is Low
			  rst  : in  STD_LOGIC;											-->
			  busy : out  STD_LOGIC;
			  addr_err : out STD_LOGIC;
			  data_exst : out STD_LOGIC);
end IP_UDP_Controller;

architecture Behavioral of IP_UDP_Controller is
	
	-- Type declarations
	type MACHINE_STATE is (STATE_WAIT_INIT, STATE_IDLE, STATE_RST , STATE_SEND , STATE_RD, STATE_WR, STATE_NEXT_PCK, STATE_ARP_RD, STATE_ARP_ANS);
	
	-- Input & Output Signals to outside of module
	signal dout_s  	: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal din_s    	: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal rd_s     	: STD_LOGIC := '0';
	signal wr_s     	: STD_LOGIC := '0';
	signal rst_s   	: STD_LOGIC := '0';
	signal send_s   	: STD_LOGIC := '0';
	signal nxt_pck_s  : STD_LOGIC := '0';
	signal busy_s   	: STD_LOGIC := '1';
	signal addr_err_s : STD_LOGIC := '0';
	
	-- Rising edge detection signals, hold previous values
	signal rd_p     : STD_LOGIC := '0';
	signal wr_p     : STD_LOGIC := '0';
	signal rst_p    : STD_LOGIC := '0';
	signal send_p   : STD_LOGIC := '0';
	signal nxt_pck_p  : STD_LOGIC := '0';
	
	-- Control & Input Signals for Sub-Module
	signal dout_c     : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal din_c      : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
	signal rd_c       : STD_LOGIC := '0';
	signal wr_c   		: STD_LOGIC := '0';
	signal nxt_pck_c  : STD_LOGIC := '0';
	signal rst_c      : STD_LOGIC := '0';
	signal send_c     : STD_LOGIC := '0';
	signal busy_c     : STD_LOGIC := '0';
	
	-- Constants 
	constant VERSION  		: STD_LOGIC_VECTOR(3 downto 0)  := "0100";
	constant IHL      		: STD_LOGIC_VECTOR(3 downto 0)  := "0101";
	constant DSCP     		: STD_LOGIC_VECTOR(5 downto 0)  := "000000";
	constant ID 				: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	constant ETHER_TYPE_IP 	: STD_LOGIC_VECTOR(15 downto 0) := (x"0800");
	constant ETHER_TYPE_ARP : STD_LOGIC_VECTOR(15 downto 0) := (x"8060");
	constant ECN 	   		: STD_LOGIC_VECTOR(1 downto 0)  := "00";
	constant FLAGS    		: STD_LOGIC_VECTOR(2 downto 0)  := "010";
	constant FRAGMENT 		: STD_LOGIC_VECTOR(12 downto 0) := (others => '0');
	constant TTL      		: STD_LOGIC_VECTOR(7 downto 0)  := (others => '1');
	constant PROTOCOL 		: STD_LOGIC_VECTOR(7 downto 0)  := "00010001";				-- 0x11 for UDP
	constant PSD_CSM  		: STD_LOGIC_VECTOR(15 downto 0)  := (others => '0');		-- Temporarily use pseudo checksum
	
	-- State variables
	signal state  				: MACHINE_STATE := STATE_WAIT_INIT;
	signal step   				: integer range 1 to 40 := 1;
	signal f_step 				: integer range 1 to 5 := 1;
	signal wr_state 			: integer range 1 to 10 := 1; 
	signal rd_state 			: integer range 1 to 3 := 1; 
	signal rd_cnt        	: integer range 1 to 12 := 1;
	signal byte_cnt 			: integer range 1 to 7 := 1;
	
	
	-- Adress signals of destination
	signal dst_ip : STD_LOGIC_VECTOR(1 to 32) := (others => '0');
	signal dst_port : STD_LOGIC_VECTOR(1 to 16) := (others => '0');

	-- Other signals
	signal wr_packet_size   	: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	signal wr_packet_size_ip   : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	signal wr_packet_size_udp  : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	signal rd_packet_size   	: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	
	-- Used for detection IP and ARP packets
	signal ether_type 			: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	signal sender_phy_adr 		: STD_LOGIC_VECTOR(47 downto 0) := (others => '0');
	signal sender_media_adr    : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
	signal target_media_adr    : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
	
	-- Used when jumping to next packet is necessary, set it, when module is idle it will do this first
	signal jmpNxtPckNecessary  : STD_LOGIC := '0';
	
	-- Checksum
	signal CSM  	: STD_LOGIC_VECTOR(19 downto 0)  := (others => '0');
	
	-- Sub_module declaration
	COMPONENT MAC_Layer_Controller
	Generic(
			mac_src : STD_LOGIC_VECTOR(1 to 48);
			mac_dst : STD_LOGIC_VECTOR(1 to 48)
	);
	PORT(
		clk : IN std_logic;
		miso : IN std_logic;
		din : IN std_logic_vector(7 downto 0);
		rd : IN std_logic;
		wr : IN std_logic;
		nxt_pck : IN std_logic;
		rst : IN std_logic;
		send : IN std_logic;          
		mosi : OUT std_logic;
		sck : OUT std_logic;
		ss : OUT std_logic;
		dout : OUT std_logic_vector(7 downto 0);
		busy : OUT std_logic
		);
	END COMPONENT;

begin

	-- Port Map for Sub-Module
	MAC_Layer_Controller_Instance: MAC_Layer_Controller
	GENERIC MAP(
		mac_src => src_mac,
		mac_dst => dst_mac
	)
	PORT MAP(
		clk => clk,
		miso => miso,
		mosi => mosi,
		sck => sck,
		ss => ss,
		dout => dout_c,
		din => din_c,
		rd => rd_c,
		wr => wr_c,
		nxt_pck => nxt_pck_c,
		rst => rst_c,
		send => send_c,
		busy => busy_c
	);
	
	process(clk) 
	
		-- Functions & Procedures
		-- Write procedure: Write any data to TX Buffer
		procedure writeData(constant data : STD_LOGIC_VECTOR(7 downto 0)) is
		begin
			if f_step = 1 then
				din_c <= data;
				f_step <= 2;
			elsif f_step = 2 then
				wr_c <= '1';
				f_step <= 3;
			elsif f_step = 3 then
				if busy_c = '1' then
					f_step <= 4;
				end if;
			elsif f_step = 4 then
				if busy_c = '0' then
					wr_c <= '0';
					f_step <= 1;
					step <= step + 1;
				end if;
			end if;
		end procedure;
		
		-- Read procedure: Take output from dout_c after one clock cyle when you called
		procedure readData is					
		begin
			if f_step = 1 then
				rd_c <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_c = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_c = '0' then
					rd_c <= '0';
					f_step <= 1;
					step <= step + 1;
				end if;
			end if;
		end procedure;
		
		-- Send data gathered in TX buffer
		procedure sendData is					
		begin
			if f_step = 1 then
				send_c <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_c = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_c = '0' then
					send_c <= '0';
					f_step <= 1;
					step <= step + 1;
				end if;
			end if;
		end procedure;
		
		-- Software Reset for ENC28J60
		procedure reset is					
		begin
			if f_step = 1 then
				rst_c <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_c = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_c = '0' then
					rst_c <= '0';
					f_step <= 1;
					step <= step + 1;
				end if;
			end if;
		end procedure; 
		
		-- Jump to next MAC packet
		procedure jmpNextPck is					
		begin
			if f_step = 1 then
				nxt_pck_c <= '1';
				f_step <= 2;
			elsif f_step = 2 then
				if busy_c = '1' then
					f_step <= 3;
				end if;
			elsif f_step = 3 then
				if busy_c = '0' then
					nxt_pck_c <= '0';
					f_step <= 1;
					step <= step + 1;
				end if;
			end if;
		end procedure; 
		
	begin
	
		if rising_edge(clk) then
			
			-- Sample inputs
			din_s <= din;
			rd_s  <= rd;
			wr_s  <= wr;
			rst_s <= rst;
			send_s <= send;
			nxt_pck_s <= nxt_pck;
			
			-- FSM
			case state is
				when STATE_WAIT_INIT =>
					if busy_c = '0' then
						busy_s <= '0';
						state <= STATE_IDLE;
					end if;
				when STATE_IDLE =>											-- Check rising edge and start operation accordingly
					if jmpNxtPckNecessary = '1' then
						jmpNxtPckNecessary  <= '0';						-- Do this once !
						state <= STATE_NEXT_PCK;
					elsif rd_s = '1' and rd_p = '0' then
						state <= STATE_RD;
						busy_s <= '1';
					elsif rst_s = '1' and rst_p = '0' then
						state <= STATE_RST;
						busy_s <= '1';
					elsif wr_s = '1' and wr_p = '0' then
						state <= STATE_WR;
						busy_s <= '1';
					elsif send_s = '1' and send_p = '0' then
						state <= STATE_SEND;
						busy_s <= '1';
					elsif nxt_pck_s = '1' and nxt_pck_p = '0' then
						state <= STATE_NEXT_PCK;
						busy_s <= '1';
				   else
						busy_s <= '0';
					end if;
				when STATE_RST =>
					if step = 1 then
						reset;
					elsif step = 2 then
						step <= 1;
						state <= STATE_WAIT_INIT;
					end if;
				when STATE_SEND =>
					if step = 1 then
						sendData;
					elsif step = 2 then
						wr_state <= 1;
						step <= 1;
						state <= STATE_IDLE;
					end if;
				when STATE_RD =>
					if step = 1 then
						case rd_state is
							when 1 => step <= 2;
							when 2 => step <= 35;
							when 3 => step <= 36;
						end case;
					-----------------------------------------------------
					elsif step = 2 then												-- Read and save ether type					
						readData;														
					elsif step = 3 then
						ether_type(15 downto 8) <= dout_c;
					elsif step = 4 then
						readData;
					elsif step = 5 then
						ether_type(7 downto 0) <= dout_c;
					elsif step = 6 then											
						if ether_type = ETHER_TYPE_IP then					   -- If this is IP packet, continue reading
							step <= 7;
						elsif ether_type = ETHER_TYPE_ARP then
							step <= 1;
							state <= STATE_ARP_RD;										-- If ARP packet has come, jump arp state
						else
							step <= 1;
							state <= STATE_NEXT_PCK;								-- Otherwise discard packet
						end if;
					---------------------------------------------------------
					elsif step = 7 then												
						readData;														-- Skip VER + IHL
					elsif step = 8 then
						readData;														-- Skip DSCP + ECN
					elsif step = 9 then												-- Read and save packet size (including IP header size)
						readData;	
					elsif step = 10 then
						rd_packet_size(15 downto 8) <= dout_c;					
						step <= 11;
					elsif step = 11 then
						readData;
					elsif step = 12 then
						rd_packet_size(7 downto 0) <= dout_c;
						step <= 13;
					elsif step = 13 then												-- Skip some parts in IPV4 packet
						readData;
					elsif step = 14 then
						if rd_cnt = 12 then
							rd_cnt <= 1;
							step <= 15;
						else
							rd_cnt <= rd_cnt + 1;
							step <= 13;
						end if;
					elsif step = 15 then
						readData;
					elsif step = 16 then											  -- For addr_err flag, check ip address
						if dout_c /= src_ip(1 to 8) then
							addr_err_s <= '1';
						end if;
						step <= 17;
					elsif step = 17 then
						readData;
					elsif step = 18 then
						if dout_c /= src_ip(9 to 16) then
							addr_err_s <= '1';
						end if;
						step <= 19;
					elsif step = 19 then
						readData;
					elsif step = 20 then
						if dout_c /= src_ip(17 to 24) then
							addr_err_s <= '1';
						end if;
						step <= 21;
					elsif step = 21 then
						readData;
					elsif step = 22 then
						if dout_c /= src_ip(25 to 32) then
							addr_err_s <= '1';
						end if;
						step <= 23;
					elsif step = 23 then												-- Skip some other parts
						readData;
					elsif step = 24 then
						readData;
					elsif step = 25 then
						readData;
					elsif step = 26 then												-- For addr_err flag, check port address										
						if dout_c /= src_port(1 to 8) then
							addr_err_s <= '1';
						end if;
						step <= 27;
					elsif step = 27 then
						readData;
					elsif step = 28 then
						if dout_c /= src_port(9 to 16) then
							addr_err_s <= '1';
						end if;
						step <= 29;
					elsif step = 29 then
						readData;											-- Skip Length, we obtain it from IP header
					elsif step = 30 then
						readData;
					elsif step = 31 then									-- Skip Checksum, optional for IPV4
						readData;
					elsif step = 32 then
						readData;
					elsif step = 33 then
						rd_packet_size <= rd_packet_size - 28; 	-- Exclude IP Header + UDP Header
						step <= 34;
					elsif step = 34 then									-- First read, skip others put packet size high byte
						rd_state <= 2;
						dout_s <= rd_packet_size(15 downto 8);
						step <= 1;
						state <= STATE_IDLE;
					elsif step = 35 then									-- Second read, put packet size low byte								
						rd_state <= 3;
						dout_s <= rd_packet_size(7 downto 0);
						step <= 1;
						state <= STATE_IDLE;
					elsif step = 36 then									-- Other reads, read data payload of IPV4
						readData;
					elsif step = 37 then
						dout_s <= dout_c;
						step <= 1;
						state <= STATE_IDLE;
					end if;
					
				when STATE_ARP_RD =>
					if step = 1 then
						readData;											-- Skip hardware type + protocol type 
					elsif step = 2 then									-- + Lengths + First byte of operation
						if byte_cnt = 7 then
							byte_cnt <= 1;
							step <= 3;
						else
							byte_cnt <= byte_cnt + 1;
							step <= 1;
						end if;
					elsif step = 3 then
						readData;											-- Read operation byte
					elsif step = 4 then	
						if dout_c = "00000001" then					-- ARP request is received
							step <= 5 	;
--						elsif dout_c = "00000010" then				-- ARP reply is received
--							step <= 1;										-- If somebody send us their MAC, just ignore it for now.
--							state <= STATE_NEXT_PCK;
						else													-- If invalid, skip packet
							step <= 1;
							state <= STATE_NEXT_PCK;
						end if;
					elsif step = 5 then									-- Save sender's MAC
						readData;
					elsif step = 6 then
						sender_phy_adr(47 downto 40) <= dout_c;
						step <= step + 1;
					elsif step = 7 then
						readData;
					elsif step = 8 then
						sender_phy_adr(39 downto 32) <= dout_c;
						step <= step + 1;
					elsif step = 9 then
						readData;
					elsif step = 10 then
						sender_phy_adr(31 downto 24) <= dout_c;
						step <= step + 1;
					elsif step = 11 then
						readData;
					elsif step = 12 then
						sender_phy_adr(23 downto 16) <= dout_c;
						step <= step + 1;
					elsif step = 13 then
						readData;
					elsif step = 14 then
						sender_phy_adr(15 downto 8) <= dout_c;
						step <= step + 1;
					elsif step = 15 then
						readData;
					elsif step = 16 then
						sender_phy_adr(7 downto 0) <= dout_c;
						step <= step + 1;
					elsif step = 17 then												-- Save senders IP
						readData;
					elsif step = 18 then
						sender_media_adr(31 downto 24) <= dout_c;
						step <= step + 1;
					elsif step = 19 then
						readData;
					elsif step = 20 then
						sender_media_adr(23 downto 16) <= dout_c;
						step <= step + 1;
					elsif step = 21 then
						readData;
					elsif step = 22 then
						sender_media_adr(15 downto 8) <= dout_c;
						step <= step + 1;
					elsif step = 23 then
						readData;
					elsif step = 24 then
						sender_media_adr(7 downto 0) <= dout_c;
						step <= step + 1;
					-------------------------------------------------------
					elsif step = 25 then
						readData;
					elsif step = 26 then										-- Skip target MAC, we will send it	
						if byte_cnt = 6 then
							step <= step + 1;
							byte_cnt <= 1;
						else
							byte_cnt <= byte_cnt + 1;
							step <= 25;
						end if;
					elsif step = 27 then
						readData;
					elsif step = 28 then
						target_media_adr(31 downto 24) <= dout_c;			-- Read target IP
						step <= step + 1;
					elsif step = 29 then
						readData;
					elsif step = 30 then
						target_media_adr(23 downto 16) <= dout_c;
						step <= step + 1;
					elsif step = 31 then
						readData;
					elsif step = 32 then
						target_media_adr(15 downto 8) <= dout_c;
						step <= step + 1;
					elsif step = 33 then
						readData;
					elsif step = 34 then
						target_media_adr(7 downto 0) <= dout_c;
						step <= step + 1;
					elsif step = 35 then
						if target_media_adr = src_ip then					-- This is sent to us, lets tell our MAC	
							step <= 1;
							state <= STATE_ARP_ANS;
						else
							step <= 1;
							state <= STATE_NEXT_PCK;							-- Not asking to us, discard packet
						end if;
					end if;
					
				when STATE_ARP_ANS =>
					if step = 1 then
						writeData("00000000");									-- Write Hardware Type
					elsif step = 2 then
						writeData("00000001");
					elsif step = 3 then
						writeData("00001000");									-- Write Protocol Type
					elsif step = 4 then
						writeData("00000000");
					elsif step = 5 then
						writeData("00000110");									-- Write Hardware Size						
					elsif step = 6 then
						writeData("00000100");									-- Write Protocol Size
					elsif step = 7 then
						writeData("00000000");									-- Write operation code (Reply:2)
					elsif step = 8 then
						writeData("00000010");
					elsif step = 9 then											-- Write our MAC
						writeData(src_mac(1 to 8));
					elsif step = 10 then
						writeData(src_mac(9 to 16));
					elsif step = 11 then											
						writeData(src_mac(17 to 24));
					elsif step = 12 then
						writeData(src_mac(25 to 32));
					elsif step = 13 then											
						writeData(src_mac(33 to 40));
					elsif step = 14 then
						writeData(src_mac(41 to 48));
					elsif step = 15 then											
						writeData(src_ip(1 to 8));								-- Write our ip
					elsif step = 16 then
						writeData(src_ip(9 to 16));
					elsif step = 17 then											
						writeData(src_ip(17 to 24));
					elsif step = 18 then
						writeData(src_ip(25 to 32));
					elsif step = 19 then											-- Write senders MAC
						writeData(sender_phy_adr(47 downto 40));
					elsif step = 20 then
						writeData(sender_phy_adr(39 downto 32));
					elsif step = 21 then											
						writeData(sender_phy_adr(31 downto 24));
					elsif step = 22 then
						writeData(sender_phy_adr(23 downto 16));
					elsif step = 23 then											
						writeData(sender_phy_adr(15 downto 8));
					elsif step = 24 then
						writeData(sender_phy_adr(7 downto 0));
					elsif step = 25 then											
						writeData(sender_media_adr(31 downto 24));				-- Write senders ip
					elsif step = 26 then
						writeData(sender_media_adr(23 downto 16));
					elsif step = 27 then											
						writeData(sender_media_adr(15 downto 8));
					elsif step = 28 then
						writeData(sender_media_adr(7 downto 0));
					elsif step = 29 then
						jmpNxtPckNecessary <= '1';
						step <= 1;
						state <= STATE_SEND;
					end if;
						
				when STATE_WR =>
					if step = 1 then
						if wr_state = 1 then										-- Get Destination IP
							dst_ip(1 to 8) <= din_s;
							wr_state <= 2;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 2 then
							dst_ip(9 to 16) <= din_s;
							wr_state <= 3;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 3 then
							dst_ip(17 to 24) <= din_s;
							wr_state <= 4;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 4 then
							dst_ip(25 to 32) <= din_s;
							wr_state <= 5;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 5 then									-- Get destination port
							dst_port(1 to 8) <= din_s;
							wr_state <= 6;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 6 then
							dst_port(9 to 16) <= din_s;
							wr_state <= 7;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 7 then									-- Get packet size to write
							wr_packet_size(15 downto 8) <= din_s;
							wr_state <= 8;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 8 then
							wr_packet_size(7 downto 0) <= din_s;
							wr_state <= 9;
							step <= 1;
							state <= STATE_IDLE;
						elsif wr_state = 9 then
							wr_packet_size_ip <= wr_packet_size + 28;		-- Add IP Header size + UDP Header Size
							wr_packet_size_udp <= wr_packet_size + 8;		-- Add UDP Header Size
							wr_state <= 10;										-- Now input will be written into data payload
							step <= 2;
						else
							step <= 39;
						end if;
					elsif step = 2 then
						writeData(ETHER_TYPE_IP(15 downto 8));					-- Write ETHERTYPE for IPV4
					elsif step = 3 then
						writeData(ETHER_TYPE_IP(7 downto 0));
					elsif step = 4 then
						writeData(VERSION & IHL);											
					elsif step = 5 then
						writeData(DSCP & ECN);
					elsif step = 6 then
						writeData(wr_packet_size_ip(15 downto 8));		-- Write packet size including IPV4 header
					elsif step = 7 then
						writeData(wr_packet_size_ip(7 downto 0));
					elsif step = 8 then
						writeData(ID(15 downto 8));
					elsif step = 9 then
						writeData(ID(7 downto 0));
					elsif step = 10 then
						writeData(FLAGS & FRAGMENT(12 downto 8));			-- No fragment, fragmentation is not supported in this version
					elsif step = 11 then
						writeData(FRAGMENT(7 downto 0));						-- Basically fragment offset will remain 0
					elsif step = 12 then
						writeData(TTL);											-- Time to live default value is 0xFF
					elsif step = 13 then
						writeData(PROTOCOL);										-- 0x11 for UDP
					----------------------------------------------------- Calculate IPV4 Header Checksum
					elsif step = 14 then											
						CSM <= "00011000010000010001";
						step <= 15;
					elsif step = 15 then
						CSM <= CSM + ("0000" & wr_packet_size_ip);
						step <= 16;
					elsif step = 16 then
						CSM <= CSM + ("0000" + src_ip(1 to 16));
						step <= 17;
					elsif step = 17 then
						CSM <= CSM + ("0000" & src_ip(17 to 32));
						step <= 18;
					elsif step = 18 then
						CSM <= CSM + ("0000" & dst_ip(1 to 16));
						step <= 19;
					elsif step = 19 then
						CSM <= CSM + ("0000" & dst_ip(17 to 32));
						step <= 20;
				   ------------------------------------------------------ Write IPV4 Header Checksum
					elsif step = 20 then														
						CSM <= CSM + ("0000000000000000" & CSM(19 downto 16));
						step <= 21;						
					elsif step = 21 then
						writeData(not CSM(15 downto 8));				
					elsif step = 22 then
						writeData(not CSM(7 downto 0));
					elsif step = 23 then   
						writeData(src_ip(1 to 8));										-- Src ip is generic !!
					elsif step = 24 then
						writeData(src_ip(9 to 16));
					elsif step = 25 then
						writeData(src_ip(17 to 24));
					elsif step = 26 then
						writeData(src_ip(25 to 32));
					elsif step = 27 then
						writeData(dst_ip(1 to 8));						 			  	-- Dest ip is input for each packet !!
					elsif step = 28 then
						writeData(dst_ip(9 to 16));
					elsif step = 29 then
						writeData(dst_ip(17 to 24));
					elsif step = 30 then
						writeData(dst_ip(25 to 32));
					----------
					elsif step = 31 then
						writeData(src_port(1 to 8));									-- Write Source Port (given as generic)
					elsif step = 32 then
						writeData(src_port(9 to 16));
					elsif step = 33 then
						writeData(dst_port(1 to 8));									-- Write Destination Port (given by user for each packet)
					elsif step = 34 then
						writeData(dst_port(9 to 16));
					elsif step = 35 then
						writeData(wr_packet_size_udp(15 downto 8));				-- Write data size for UDP
					elsif step = 36 then
						writeData(wr_packet_size_udp(7 downto 0));	
					elsif step = 37 then
						writeData(PSD_CSM(15 downto 8));								-- Write placeholder for checksum
					elsif step = 38 then
						writeData(PSD_CSM(7 downto 0));
					----------
					elsif step = 39 then
						writeData(din_s);													-- Write data given by user
					elsif step = 40 then
						step <= 1;
						state <= STATE_IDLE;
					end if;
					
			   when STATE_NEXT_PCK => 
					if step = 1 then
						jmpNextPck;
					elsif step = 2 then
						rd_state <= 1;
						addr_err_s <= '0';
						step <= 1;
						state <= STATE_IDLE;
					end if;
			end case;
			
			-- Save previous values
			rd_p  <= rd_s;
			wr_p  <= wr_s;
			rst_p <= rst_s;
			send_p <= send_s;
			nxt_pck_p <= nxt_pck_s;
			
			-- Assert outputs
			dout <= dout_s;
			busy <= busy_s;
			addr_err <= addr_err_s;
			
		end if;		
	end process;
	
	-- Convey interrupt signal of ENC28J60 as data exist
	data_exst <= not int;
	
end Behavioral;
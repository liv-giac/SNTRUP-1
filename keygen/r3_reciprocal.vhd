library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use ieee.math_real.all;

use work.constants.all;
use work.data_type.all;


-- Calculates the reciprocal of a polynomial in r3
entity r3_reciprocal is
	port(
		clock               : in  std_logic;
		reset               : in  std_logic;
		start               : in  std_logic;
		small_polynomial_in : in  std_logic_vector(1 downto 0);
		ready               : out std_logic;
		output_polynomial   : out std_logic_vector(1 downto 0);
		output_valid        : out std_logic;
		is_invertable       : out std_logic;
		done                : out std_logic
	);
end entity r3_reciprocal;

architecture RTL of r3_reciprocal is
	constant loop_limit : integer := 2 * p - 1;
	constant b : integer := 4;
	constant bram_address_width : integer :=  integer(ceil(log2(real((p + 1)/(b+1)))));
	type address_vector is array (0 to b) of std_logic_vector(bram_address_width - 1 downto 0);
	type data_vector is array (0 to b) of std_logic_vector(1 downto 0);
	type freeze_vector is array (0 to b) of signed(1 downto 0);
	
	signal counter             : integer range 0 to loop_limit + 1 := 0;
	type state_type is (init_state, reset_ram, reset_ram_end, ready_state, running_state, swap_state_1, swap_state_2, swap_state_3, multiply_state_read, multiply_final_state_1, multiply_final_state_2, calc_reciprocal_init, calc_reciprocal_init_2, calc_reciprocal, output_data, done_state);
	signal state_r3_reciprocal : state_type;

	signal counter_vr : integer range 0 to p + 2;
	signal counter_fg : integer range 0 to p + 2;

	signal bram_f_write_b_reset : std_logic_vector(b downto 0);
	signal bram_g_write_b_reset : std_logic_vector(b downto 0);
	signal bram_v_write_b_reset : std_logic_vector(b downto 0);
	signal bram_r_write_b_reset : std_logic_vector(b downto 0);

	signal bram_f_data_in_b_reset : data_vector;
	signal bram_g_data_in_b_reset : data_vector;
	signal bram_v_data_in_b_reset : data_vector;
	signal bram_r_data_in_b_reset : data_vector;

	signal bram_f_address_b_reset : address_vector;
	signal bram_g_address_b_reset : address_vector;
	signal bram_v_address_b_reset : address_vector;
	signal bram_r_address_b_reset : address_vector;

	signal bram_f_address_a : address_vector;
	signal bram_g_address_a : address_vector;
	signal bram_v_address_a : address_vector;
	signal bram_r_address_a : address_vector;

	signal bram_f_data_out_a : data_vector;
	signal bram_g_data_out_a : data_vector;
	signal bram_v_data_out_a : data_vector;
	signal bram_r_data_out_a : data_vector;

	signal swap_mask_s : std_logic := '0';



	signal f_zero : std_logic_vector(1 downto 0);
	signal g_zero : std_logic_vector(1 downto 0);

	signal fg_freeze : freeze_vector;

	signal bram_f_data_in_b : data_vector;
	signal bram_g_data_in_b : data_vector;
	signal bram_v_data_in_b : data_vector;
	signal bram_r_data_in_b : data_vector;

	signal bram_f_write_b : std_logic_vector(b downto 0);
	signal bram_g_write_b : std_logic_vector(b downto 0);
	signal bram_v_write_b : std_logic_vector(b downto 0);
	signal bram_r_write_b : std_logic_vector(b downto 0);

	signal bram_f_address_b : address_vector;
	signal bram_g_address_b : address_vector;
	signal bram_v_address_b : address_vector;
	signal bram_r_address_b : address_vector;

	constant pipeline_length : integer := 1;

	type address_delay_vec is array (pipeline_length downto 0) of address_vector;
	type write_delay_vec is array (pipeline_length downto 0) of std_logic_vector (b downto 0);

	signal bram_g_address_b_delay : address_delay_vec;
	signal bram_g_write_b_delay   : write_delay_vec;

	signal vr_freeze : freeze_vector;

	signal bram_r_address_b_delay : address_delay_vec;
	signal bram_r_write_b_delay   : write_delay_vec;

	-- Shift data in v RAM
	signal bram_shift_v_address_b : address_vector;
	signal bram_shift_v_data_in_b : data_vector;
	signal bram_shift_v_write_b   : std_logic_vector(b downto 0);

	signal reciprocal_output : std_logic_vector(1 downto 0);

	signal output_freeze : signed(1 downto 0);

	signal output_valid_pipe : std_logic_vector(pipeline_length downto 0);
	signal g_data_out_a0_reg : std_logic_vector(1 downto 0);

	signal v_g0_inter : data_vector;
	signal r_f0_inter : data_vector;

	signal f_g0_inter : data_vector;
	signal g_f0_inter : data_vector;




begin
	

	main : process(clock, reset) is
		variable delta : signed(15 downto 0);
		variable extra : integer;
		variable swap_mask : signed(15 downto 0);
		
	begin
		if reset = '1' then
			state_r3_reciprocal <= init_state;
			
			bram_g_address_b_delay(0) <= (others => (others => '0'));
			bram_g_write_b_delay(0)   <= (others => '0');

			bram_r_address_b_delay(0) <= (others => (others => '0'));
			bram_r_write_b_delay(0)   <= (others => '0');

			bram_f_write_b_reset <= (others => '0');
			bram_g_write_b_reset <= (others => '0');
			bram_v_write_b_reset <= (others => '0');
			bram_r_write_b_reset <= (others => '0');

			bram_f_data_in_b_reset <= (others => (others => '0'));
			bram_g_data_in_b_reset <= (others => (others => '0'));
			bram_v_data_in_b_reset <= (others => (others => '0'));
			bram_r_data_in_b_reset <= (others => (others => '0'));

			f_zero <= (others => '0');
			g_zero <= (others => '0');

			bram_shift_v_write_b    <= (others => '0');
			bram_r_write_b_delay(0) <= (others => '0');

			done          <= '0';
			is_invertable <= '0';

			
			output_valid_pipe(0) <= '0';
		elsif rising_edge(clock) then
			case state_r3_reciprocal is
				when init_state =>
					state_r3_reciprocal  <= ready_state;
					delta                := to_signed(1, 16);
					swap_mask            := (others => '0');
					counter              <= 0;
					counter_vr           <= 0;
					counter_fg           <= 0;
					
					output_valid_pipe(0) <= '0';
					ready                <= '0';
					swap_mask_s          <= '0';
					done                 <= '0';
					is_invertable        <= '0';
				when ready_state =>
					if start = '1' then
						state_r3_reciprocal <= reset_ram;
						ready               <= '0';
					else
						state_r3_reciprocal <= ready_state;
						ready               <= '1';
					end if;
					bram_f_write_b_reset <= (others => '0');
					bram_g_write_b_reset <= (others => '0');
					bram_v_write_b_reset <= (others => '0');
					bram_r_write_b_reset <= (others => '0');
				when reset_ram =>  ---still saves one coefficient at a time but in banked logic
					extra := b + 1 - (p+1) mod (b+1);
					---added a reset since this time it's not the same bram being written to every iteration, but every b + 1 times


					bram_f_write_b_reset <= (others => '0');
					bram_g_write_b_reset <= (others => '0');
					bram_v_write_b_reset <= (others => '0');
					bram_r_write_b_reset <= (others => '0');
					
				    bram_f_address_b_reset(counter_fg mod (b+1)) <= std_logic_vector(to_unsigned(integer(counter_fg/(b+1)), bram_address_width));
					bram_g_address_b_reset((p - 1 - counter_fg) mod (b+1)) <= std_logic_vector(to_signed(integer((p - 1 - counter_fg)/(b+1)), bram_address_width + 1)(bram_address_width - 1 downto 0));

					
					bram_v_address_b_reset(counter_vr mod (b+1)) <= std_logic_vector(to_unsigned(integer(counter_vr/(b+1)), bram_address_width));
					bram_r_address_b_reset(counter_vr mod (b+1)) <= std_logic_vector(to_unsigned(integer(counter_vr/(b+1)), bram_address_width));

					if counter_fg = 0 then	 
						bram_f_data_in_b_reset(counter_fg mod (b+1)) <= std_logic_vector(to_signed(1, 2));
					elsif counter_fg = p or counter_fg = p - 1 then
						bram_f_data_in_b_reset(counter_fg mod (b+1)) <= std_logic_vector(to_signed(-1, 2));
					else
						bram_f_data_in_b_reset(counter_fg mod (b+1)) <= (others => '0');
					end if;

					if counter_fg < p then
						bram_g_data_in_b_reset((p - 1 - counter_fg) mod (b+1)) <= std_logic_vector(resize(signed(small_polynomial_in), 2));
			
					else
						bram_g_data_in_b_reset(p mod (b+1)) <= (others => '0');
						bram_g_address_b_reset(p mod (b+1)) <= std_logic_vector(to_unsigned(integer(p/(b+1)), bram_address_width));
					
					end if;

					bram_v_data_in_b_reset(counter_vr mod (b+1)) <= (others => '0');

					if counter_vr = 0 then
						bram_r_data_in_b_reset(counter_vr mod (b+1)) <= std_logic_vector(to_signed(1, 2));
					else
						bram_r_data_in_b_reset(counter_vr mod (b+1)) <= (others => '0');

					end if;
					
					bram_f_write_b_reset(counter_fg mod (b+1)) <= '1';
					if counter_fg = p then
					bram_g_write_b_reset((p) mod (b+1)) <= '1';
						else
					bram_g_write_b_reset((p - 1 - counter_fg) mod (b+1)) <= '1';
					end if;
					bram_v_write_b_reset(counter_vr mod (b+1)) <= '1';
					bram_r_write_b_reset(counter_vr mod (b+1)) <= '1';
					





					counter_fg <= counter_fg + 1;
					counter_vr <= counter_vr + 1;
					if counter_fg < p + 1 then
						state_r3_reciprocal <= reset_ram;

					else
						state_r3_reciprocal <= reset_ram_end;
					end if; 


				
				when reset_ram_end =>
				
						state_r3_reciprocal     <= running_state;
						
							bram_g_address_b_reset          <=  (others => (others => '0'));
							bram_f_address_b_reset          <=  (others => (others => '0'));
							bram_g_write_b_delay(0)         <=  (others => '0');
							bram_r_write_b_delay(0)         <=  (others => '0');
							bram_shift_v_write_b            <=  (others => '0');
							bram_f_write_b_reset            <=  (others => '0');
							bram_g_write_b_reset            <=  (others => '0');
							bram_v_write_b_reset            <=  (others => '0');
							bram_r_write_b_reset            <=  (others => '0');

				when running_state =>
				
					if counter >= loop_limit then
						state_r3_reciprocal <= calc_reciprocal_init;
					else
						state_r3_reciprocal <= swap_state_1;
					end if;
					bram_g_address_b_reset <= (others => (others => '0'));
					bram_f_address_b_reset <= (others => (others => '0'));

					counter                 <= counter + 1;
					
					counter_fg              <= 1;
					counter_vr              <= 0;
					bram_g_write_b_delay(0) <= (others => '0');
					bram_r_write_b_delay(0) <= (others => '0');
					bram_shift_v_write_b    <= (others => '0');
					
					extra:=0;
				when swap_state_1 =>


					state_r3_reciprocal <= swap_state_2;
			
			
				when swap_state_2 =>


					---it's always the first value of g so in the first bank
					swap_mask := negative_mask(-delta) AND non_zero_mask(signed(bram_g_data_out_a(0)));


					state_r3_reciprocal <= swap_state_3;

					delta := (delta XOR (swap_mask AND (delta XOR -delta))) + to_signed(1, 16);

					 if swap_mask(0) = '1' then
						swap_mask_s <= not swap_mask_s;
					else
						swap_mask_s <= swap_mask_s;
					end if;




				when swap_state_3 =>
					---it's always the first value of g and f so in the first bank
					state_r3_reciprocal <= multiply_state_read;
					f_zero              <= bram_f_data_out_a(0);
					g_zero              <= bram_g_data_out_a(0);
				when multiply_state_read =>	

					-- calculation to deal with the last iteration if the batching has a "remainder"
					if counter_fg + (b+1) > p + 1 then
										extra := (p+1) mod (b+1);				
						else
							extra := b + 1;
						end if;

					
					for i in 0 to extra - 1 loop
						
					    bram_f_address_a((counter_fg+i) mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_fg+i)/ (b+1)), bram_address_width));
						bram_g_address_a((counter_fg+i) mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_fg+i)/ (b+1)), bram_address_width));
						
						bram_v_address_a((counter_vr+i)mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_vr+i)/ (b+1)), bram_address_width));
						bram_r_address_a((counter_vr+i)mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_vr+i)/ (b+1)), bram_address_width));
										

						bram_g_address_b_delay(0)((counter_fg+i) mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_fg+i)/ (b+1)), bram_address_width));
						bram_g_write_b_delay(0)((counter_fg+i) mod (b+1))   <= '1';

						bram_r_address_b_delay(0)((counter_vr+i)mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_vr+i)/ (b+1)), bram_address_width));
						bram_r_write_b_delay(0)((counter_vr+i)mod (b+1))   <= '1';
					end loop;
					

					counter_fg <= counter_fg  + extra;
					counter_vr <= counter_vr  + extra;
					if counter_fg + extra >= p+1 then
						state_r3_reciprocal <= multiply_final_state_1;
					else
						state_r3_reciprocal <= multiply_state_read;
					end if;

						--shifting logic on batched ram needs to take into account that for b out of b+1 elements the address doesn't change, only the bram index changes,
					--instead for the final one the address changes because we have wraparound
					if counter = loop_limit then
							bram_shift_v_write_b <= (others => '0');
						else
							bram_shift_v_write_b <= (others => '0');
							if counter_vr = extra then
								bram_shift_v_address_b(0) <= (others => '0');
								bram_shift_v_data_in_b(0) <= (others => '0');
								bram_shift_v_write_b(0)    <= '1';	
							else
								if counter_vr > extra then
									

									for i in 0 to extra - 1 loop
										bram_shift_v_address_b((i+1)mod (b+1)) <= std_logic_vector(to_unsigned(integer((counter_vr+i)/(b+1))-2, bram_address_width));
										bram_shift_v_data_in_b((i+1)mod (b+1)) <= bram_v_data_out_a(i);
										bram_shift_v_write_b((i+1)mod (b+1))  <= '1';
										
									end loop;
									bram_shift_v_write_b(0)  <= '0';
									if extra = b+1 then
										bram_shift_v_data_in_b(0) <= bram_v_data_out_a(b);
										bram_shift_v_address_b(0) <= std_logic_vector(to_unsigned(integer((counter_vr+b)/(b+1))-1, bram_address_width));
										bram_shift_v_write_b(0)  <= '1';
									
										
									end if;
										
								end if;
							end if;
					end if;
					
								
				when multiply_final_state_1 =>
					bram_g_write_b_delay(0) <= (others => '0');
					bram_r_write_b_delay(0) <= (others => '0');
					bram_shift_v_write_b   <= (others => '0');
					bram_f_address_a <= (others => (others => '0'));
					bram_g_address_a <= (others => (others => '0'));
					
					state_r3_reciprocal     <= multiply_final_state_2;
				when multiply_final_state_2 =>
					state_r3_reciprocal     <= running_state;
					bram_shift_v_write_b    <= (others => '0');
					bram_g_write_b_delay(0) <= (others => '0');
					bram_v_address_a <= (others => (others => '0'));
					bram_r_address_a <= (others => (others => '0'));
				
				when calc_reciprocal_init =>
					state_r3_reciprocal <= calc_reciprocal_init_2;
					bram_f_address_a    <= (others => (others => '0'));
				when calc_reciprocal_init_2 =>
					state_r3_reciprocal <= calc_reciprocal;
					
				when calc_reciprocal =>
					reciprocal_output   <= bram_f_data_out_a(0);
					counter_vr          <= 0;
					state_r3_reciprocal <= output_data;
					
				when output_data =>
				    
						-- outputs one coefficient at a time moving across brams
					
					bram_v_address_a((p - counter_vr-1) mod (b+1)) <= std_logic_vector(to_signed(integer((p - 1 - counter_vr)/(b+1)), bram_address_width + 1)(bram_address_width - 1 downto 0));
					counter_vr           <= counter_vr + 1;
					
					output_valid_pipe(0) <= '1';
					if counter_vr < p then
						state_r3_reciprocal <= output_data;
					else
						state_r3_reciprocal  <= done_state;
						output_valid_pipe(0) <= '0';
					end if;
					if non_zero_mask(delta) = 0 then
						is_invertable <= '1';
						

					end if;
				when done_state =>
					state_r3_reciprocal  <= init_state;
					output_valid_pipe(0) <= '0';
					done                 <= '1';
			end case;
		end if;
	end process main;



						--makes sure to get the right bram index out
	output_freeze <= "00" when reciprocal_output = "00" or bram_v_data_out_a((p -1 - counter_vr) mod (b+1)) = "00"
		else "01" when (reciprocal_output = "01" and bram_v_data_out_a((p - 1 -counter_vr) mod (b+1)) = "01") or (reciprocal_output = "11" and bram_v_data_out_a((p - counter_vr) mod (b+1)) = "11")
		else "11" when (reciprocal_output = "11" and bram_v_data_out_a((p - 1 - counter_vr) mod (b+1)) = "01") or (reciprocal_output = "01" and bram_v_data_out_a((p - counter_vr) mod (b+1)) = "11")
		else "00";

	output_polynomial <= std_logic_vector(output_freeze);

	delay_output_valid : process(clock, reset) is
	begin
		if reset = '1' then
			output_valid_pipe(pipeline_length downto 1) <= (others => '0');
		elsif rising_edge(clock) then
			output_valid_pipe(pipeline_length downto 1) <= output_valid_pipe(pipeline_length - 1 downto 0);
		end if;
	end process delay_output_valid;

	output_valid <= output_valid_pipe(pipeline_length);

	-- Multiplication of f0*g[i]-g0*f[i]
	g_f0_inter_gen : for i in 0 to b generate
		g_f0_inter(i) <= "00" when bram_g_data_out_a(i) = "00" or f_zero = "00"
			else "01" when (bram_g_data_out_a(i) = "01" and f_zero = "01") or (bram_g_data_out_a(i) = "11" and f_zero = "11")
			else "11" when (bram_g_data_out_a(i) = "11" and f_zero = "01") or (bram_g_data_out_a(i) = "01" and f_zero = "11")
			else "00";
	end generate;

	f_g0_inter_gen : for i in 0 to b generate	
		f_g0_inter(i) <= "00" when bram_f_data_out_a(i) = "00" or g_zero = "00"
			else "01" when (bram_f_data_out_a(i) = "01" and g_zero = "01") or (bram_f_data_out_a(i) = "11" and g_zero = "11")
			else "11" when (bram_f_data_out_a(i) = "11" and g_zero = "01") or (bram_f_data_out_a(i) = "01" and g_zero = "11")
			else "00";
	end generate;

	fg_freeze_gen : for i in 0 to b generate
		fg_freeze(i) <= "00" when g_f0_inter(i) = f_g0_inter(i)
			else "01" when (g_f0_inter(i) = "01" and f_g0_inter(i) = "00") or (g_f0_inter(i) = "11" and f_g0_inter(i) = "01") or (g_f0_inter(i) = "00" and f_g0_inter(i) = "11")
			else "11" when (g_f0_inter(i) = "00" and f_g0_inter(i) = "01") or (g_f0_inter(i) = "01" and f_g0_inter(i) = "11") or (g_f0_inter(i) = "11" and f_g0_inter(i) = "00")
			else "00";
	end generate;
	

	-- Delay the write to g bram to wait for freeze pipeline to complete.
	-- Also shifts the address by one to implement the shift of g
			--shifting logic on batched ram needs to take into account that for b out of b+1 elements the address doesn't change, only the bram index changes,
			--instead for the first one the address changes because we have wraparound
	delay_bram_g_port_b : process(clock, reset)
	begin
		if reset = '1' then

				bram_g_address_b_delay(pipeline_length downto 1) <=  (others =>(others =>(others => '0')));
				bram_g_write_b_delay(pipeline_length downto 1)   <= (others =>(others =>'0'));

		else 

			if rising_edge(clock) then
				
				bram_g_address_b_delay(1)(b) <= std_logic_vector(signed(bram_g_address_b_delay(0)(0)) - to_signed(1, bram_address_width));
				if bram_g_address_b_delay(0)(0) = std_logic_vector(to_unsigned(0, bram_address_width)) then
					bram_g_write_b_delay(1)(b) <= '0';
				else
					bram_g_write_b_delay(1)(b) <= bram_g_write_b_delay(0)(0);
				end if;

				for i in 1 to b loop
					bram_g_address_b_delay(1)(i-1) <= bram_g_address_b_delay(0)(i);
					bram_g_write_b_delay(1)(i-1) <= bram_g_write_b_delay(0)(i);
				end loop;
			end if;

		end if;
	end process;


	-- Multiplication of f0*r[i]-g0*v[i]
	r_f0_inter_gen : for i in 0 to b generate
		r_f0_inter(i) <= "00" when bram_r_data_out_a(i) = "00" or f_zero = "00"
			else "01" when (bram_r_data_out_a(i) = "01" and f_zero = "01") or (bram_r_data_out_a(i) = "11" and f_zero = "11")
			else "11" when (bram_r_data_out_a(i) = "11" and f_zero = "01") or (bram_r_data_out_a(i) = "01" and f_zero = "11")
			else "00";
		end generate;

	v_g0_inter_gen : for i in 0 to b generate
		v_g0_inter(i) <= "00" when bram_v_data_out_a(i) = "00" or g_zero = "00"
			else "01" when (bram_v_data_out_a(i) = "01" and g_zero = "01") or (bram_v_data_out_a(i) = "11" and g_zero = "11")
			else "11" when (bram_v_data_out_a(i) = "11" and g_zero = "01") or (bram_v_data_out_a(i) = "01" and g_zero = "11")
			else "00";
		end generate;

	vr_freeze_gen : for i in 0 to b generate
		vr_freeze(i) <= "00" when r_f0_inter(i) = v_g0_inter(i)
			else "01" when (r_f0_inter(i) = "01" and v_g0_inter(i) = "00") or (r_f0_inter(i) = "11" and v_g0_inter(i) = "01") or (r_f0_inter(i) = "00" and v_g0_inter(i) = "11")
			else "11" when (r_f0_inter(i) = "00" and v_g0_inter(i) = "01") or (r_f0_inter(i) = "01" and v_g0_inter(i) = "11") or (r_f0_inter(i) = "11" and v_g0_inter(i) = "00")
			else "00";
		end generate;


	
    delay_bram_r_port_b : process(clock, reset)
	begin
		if reset = '1' then
			
				bram_r_address_b_delay(pipeline_length downto 1) <=  (others =>(others => (others => '0')));
				bram_r_write_b_delay(pipeline_length downto 1)  <= (others =>(others =>'0'));
			
		else
			if rising_edge(clock) then
			
				bram_r_address_b_delay(pipeline_length downto 1) <= bram_r_address_b_delay(pipeline_length - 1 downto 0);
				bram_r_write_b_delay(pipeline_length downto 1)   <= bram_r_write_b_delay(pipeline_length - 1 downto 0);
			end if;
		end if;
	end process;


	bram_gen : for i in 0 to b generate
		bram_f_data_in_b(i) <= (others => '0') when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_f_data_in_b_reset(i);

		bram_g_data_in_b(i) <= bram_g_data_in_b_reset(i) when (state_r3_reciprocal = reset_ram or state_r3_reciprocal = reset_ram_end) 
			else std_logic_vector(fg_freeze(i)) when (bram_g_address_b_delay(pipeline_length)(i) /= std_logic_vector(to_unsigned(integer(p/(b+1)), bram_address_width)))
			else (others => '0');
		
		bram_v_data_in_b(i) <= bram_shift_v_data_in_b(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_v_data_in_b_reset(i);
		bram_r_data_in_b(i) <= std_logic_vector(vr_freeze(i)) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end  else bram_r_data_in_b_reset(i);

		bram_f_write_b(i) <= '0' when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_f_write_b_reset(i);
		bram_g_write_b(i) <= bram_g_write_b_delay(pipeline_length)(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_g_write_b_reset(i);
		bram_v_write_b(i) <= bram_shift_v_write_b(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_v_write_b_reset(i);
		bram_r_write_b(i) <= bram_r_write_b_delay(pipeline_length)(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_r_write_b_reset(i);

		bram_f_address_b(i) <= (others => '0') when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end  else bram_f_address_b_reset(i);
		bram_g_address_b(i) <= bram_g_address_b_delay(pipeline_length)(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_g_address_b_reset(i);
		bram_v_address_b(i) <= bram_shift_v_address_b(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_v_address_b_reset(i);
		bram_r_address_b(i) <= bram_r_address_b_delay(pipeline_length)(i) when state_r3_reciprocal /= reset_ram and state_r3_reciprocal /= reset_ram_end else bram_r_address_b_reset(i);
	end generate;

	bram_r3_reciprocal_gen : for i in 0 to b generate
    bram_r3_reciprocal_i : entity work.bram_r3_reciprocal
        generic map( 	
            bram_address_width => bram_address_width,
            bram_data_width    => 2
        )
        port map(
            clock             => clock,
            swap_mask_s       => swap_mask_s,
            bram_f_address_a  => bram_f_address_a(i),
            bram_g_address_a  => bram_g_address_a(i),
            bram_v_address_a  => bram_v_address_a(i),
            bram_r_address_a  => bram_r_address_a(i),
            bram_f_data_out_a => bram_f_data_out_a(i),
            bram_g_data_out_a => bram_g_data_out_a(i),
            bram_v_data_out_a => bram_v_data_out_a(i),
            bram_r_data_out_a => bram_r_data_out_a(i),
            bram_f_data_in_b  => bram_f_data_in_b(i),
            bram_g_data_in_b  => bram_g_data_in_b(i),
            bram_v_data_in_b  => bram_v_data_in_b(i),
            bram_r_data_in_b  => bram_r_data_in_b(i),
            bram_f_write_b    => bram_f_write_b(i),
            bram_g_write_b    => bram_g_write_b(i),
            bram_v_write_b    => bram_v_write_b(i),
            bram_r_write_b    => bram_r_write_b(i),
            bram_f_address_b  => bram_f_address_b(i),
            bram_g_address_b  => bram_g_address_b(i),
            bram_v_address_b  => bram_v_address_b(i),
            bram_r_address_b  => bram_r_address_b(i)
        );
end generate;

end architecture RTL;
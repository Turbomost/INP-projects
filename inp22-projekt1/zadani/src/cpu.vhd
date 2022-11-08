-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Václav Valenta <xvalen29 AT stud.fit.vutbr.cz>
--

-----------------------------
-- While, do-while counter --
-----------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY CNT IS
  PORT (
    CLK : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    CNT_INC : IN STD_LOGIC;
    CNT_DEC : IN STD_LOGIC;
    CNT_CLR : IN STD_LOGIC;
    CNT_SET : IN STD_LOGIC;
    CNT_OUT : OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
  );
END ENTITY CNT;

-----------------------------
ARCHITECTURE behavioral OF CNT IS
  SIGNAL cnt_value : STD_LOGIC_VECTOR(11 DOWNTO 0);
BEGIN
  cnt : PROCESS (RESET, CLK, CNT_INC, CNT_DEC, CNT_CLR, CNT_SET) BEGIN
    IF (RESET = '1') THEN
      cnt_value <= (OTHERS => '0');
    ELSIF rising_edge(CLK) THEN
      IF (CNT_CLR = '1') THEN
        cnt_value <= (OTHERS => '0');
      ELSIF (CNT_SET = '1') THEN
        cnt_value <= "000000000001";
      ELSIF (CNT_DEC = '1') THEN
        cnt_value <= cnt_value - 1;
      ELSIF (CNT_INC = '1') THEN
        cnt_value <= cnt_value + 1;
      END IF;
    END IF;
  END PROCESS;

  CNT_OUT <= cnt_value;
END behavioral;

---------------------
-- Program counter --
---------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY PC IS
  PORT (
    CLK : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    PC_INC : IN STD_LOGIC;
    PC_DEC : IN STD_LOGIC;
    PC_CLR : IN STD_LOGIC;
    PC_OUT : OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
  );
END ENTITY PC;

---------------------
ARCHITECTURE behavioral OF PC IS
  SIGNAL pc_value : STD_LOGIC_VECTOR(11 DOWNTO 0);
BEGIN
  pc : PROCESS (RESET, CLK, PC_INC, PC_DEC, PC_CLR) BEGIN
    IF (RESET = '1') THEN
      pc_value <= (OTHERS => '0');
    ELSIF rising_edge(CLK) THEN
      IF (PC_CLR = '1') THEN
        pc_value <= (OTHERS => '0');
      ELSIF (PC_DEC = '1') THEN
        pc_value <= pc_value - 1;
      ELSIF (PC_INC = '1') THEN
        pc_value <= pc_value + 1;
      END IF;
    END IF;
  END PROCESS;

  PC_OUT <= pc_value;
END behavioral;

-----------------
-- Mem pointer --
-----------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY PTR IS
  PORT (
    CLK : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    PTR_INC : IN STD_LOGIC;
    PTR_DEC : IN STD_LOGIC;
    PTR_CLR : IN STD_LOGIC;
    PTR_OUT : OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
  );
END ENTITY PTR;

-----------------
ARCHITECTURE behavioral OF PTR IS
  SIGNAL ptr_value : STD_LOGIC_VECTOR (11 DOWNTO 0);
BEGIN
  ptr : PROCESS (CLK, RESET, PTR_INC, PTR_DEC, PTR_CLR) BEGIN
    IF (RESET = '1') THEN
      ptr_value <= (OTHERS => '0');
    ELSIF rising_edge(CLK) THEN
      IF (PTR_CLR = '1') THEN
        ptr_value <= (OTHERS => '0');
      ELSIF (PTR_DEC = '1') THEN
        IF (ptr_value = "000000000000") THEN
          ptr_value <= (OTHERS => '1');
        ELSE
          ptr_value <= ptr_value - 1;
        END IF;
      ELSIF (PTR_INC = '1') THEN
        IF (ptr_value = "111111111111") THEN
          ptr_value <= (OTHERS => '0');
        ELSE
          ptr_value <= ptr_value + 1;
        END IF;
      END IF;
    END IF;
  END PROCESS;

  PTR_OUT <= ptr_value;
END behavioral;

---------
-- MX1 --
---------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY MX1 IS
  PORT (
    CLK : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    MX1_IN_PC : IN STD_LOGIC_VECTOR (11 DOWNTO 0);
    MX1_IN_PTR : IN STD_LOGIC_VECTOR (11 DOWNTO 0);
    MX1_SEL : IN STD_LOGIC;
    MX1_OUT : OUT STD_LOGIC_VECTOR (12 DOWNTO 0)
  );
END ENTITY MX1;

---------
ARCHITECTURE behavioral OF MX1 IS
  SIGNAL mx1_val : STD_LOGIC_VECTOR (12 DOWNTO 0);
BEGIN
  mx1 : PROCESS (CLK, RESET, MX1_IN_PC, MX1_IN_PTR, MX1_SEL) BEGIN
    IF (RESET = '1') THEN
      mx1_val <= (OTHERS => '0');
    ELSIF rising_edge(CLK) THEN
      CASE MX1_SEL IS
        WHEN '0' => mx1_val <= '0' & MX1_IN_PC;
        WHEN '1' => mx1_val <= '1' & MX1_IN_PTR;
        WHEN OTHERS => mx1_val <= (OTHERS => '0');
      END CASE;
    END IF;
  END PROCESS;

  MX1_OUT <= mx1_val;
END behavioral;

---------
-- MX2 --
---------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY MX2 IS
  PORT (
    CLK : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    MX2_IN_INPUT : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    MX2_IN_RDATA : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    MX2_SEL : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
    MX2_OUT : OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
  );
END ENTITY MX2;

---------
ARCHITECTURE behavioral OF MX2 IS
  SIGNAL mx2_val : STD_LOGIC_VECTOR (7 DOWNTO 0);
BEGIN
  mx2 : PROCESS (CLK, RESET, MX2_IN_INPUT, MX2_IN_RDATA, MX2_SEL) BEGIN
    IF (RESET = '1') THEN
      mx2_val <= (OTHERS => '0');
    ELSIF rising_edge(CLK) THEN
      CASE MX2_SEL IS
        WHEN "00" => mx2_val <= MX2_IN_INPUT;
        WHEN "01" => mx2_val <= MX2_IN_RDATA - 1;
        WHEN "10" => mx2_val <= MX2_IN_RDATA + 1;
        WHEN OTHERS => mx2_val <= (OTHERS => '0');
      END CASE;
    END IF;
  END PROCESS;

  MX2_OUT <= mx2_val;
END behavioral;

---------
-- FSM --
---------
LIBRARY ieee;

USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY FSM IS
  PORT (
    CLK : IN STD_LOGIC;
    RESET : IN STD_LOGIC;
    EN : IN STD_LOGIC;
    IN_VLD : IN STD_LOGIC;
    OUT_BUSY : IN STD_LOGIC;
    DATA_RDATA : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    DATA_RDWR : OUT STD_LOGIC := '0';

    CNT_INC : OUT STD_LOGIC := '0';
    CNT_DEC : OUT STD_LOGIC := '0';
    CNT_CLR : OUT STD_LOGIC := '0';
    CNT_SET : OUT STD_LOGIC := '0';

    CNT_OUT : IN STD_LOGIC_VECTOR (11 DOWNTO 0);

    PC_INC : OUT STD_LOGIC := '0';
    PC_DEC : OUT STD_LOGIC := '0';
    PC_CLR : OUT STD_LOGIC := '0';

    PTR_INC : OUT STD_LOGIC := '0';
    PTR_DEC : OUT STD_LOGIC := '0';
    PTR_CLR : OUT STD_LOGIC := '0';

    DATA_EN : OUT STD_LOGIC := '0';
    IN_REQ : OUT STD_LOGIC := '0';
    OUT_WE : OUT STD_LOGIC := '0';

    MX1_SEL : OUT STD_LOGIC := '0';
    MX2_SEL : OUT STD_LOGIC_VECTOR (1 DOWNTO 0) := "00"

  );
END ENTITY FSM;

---------
ARCHITECTURE behavioral OF FSM IS

  TYPE fsm_state IS (
    state_idle,
    state_load,
    state_decode,

    -- ptr++
    state_ptr_inc,

    -- ptr--
    state_ptr_dec,

    -- Value++
    state_val_inc,
    state_mx2_10,
    state_val_inc_end,

    -- Value--
    state_val_dec,
    state_mx2_01,
    state_val_dec_end,

    -- Putchar
    state_putchar_start,
    state_putchar_end,

    -- Getchar
    state_getchar_start,
    state_getchar_end,
    state_write,

    --  While
    state_while_do_start,
    state_while_do_start_data,
    state_while_do_start_cnt,
    state_while_do_start_cnt_reload,
    state_while_do_start_wait,
    state_while_do_start_wait2,
    state_while_do_start_cnt_end,

    state_while_do_end,
    state_while_do_end_data,
    state_while_do_end_cnt,
    state_while_do_end_cnt_reload,
    state_while_do_end_wait,
    state_while_do_end_wait2,
    state_while_do_end_cnt_end,

    -- Do-while
    state_do_while_start,

    state_do_while_end,
    state_do_while_end_data,
    state_do_while_end_cnt,
    state_do_while_end_cnt_reload,
    state_do_while_end_wait,
    state_do_while_end_wait2,
    state_do_while_end_cnt_end,

    -- Other
    state_return,
    state_undefined

  );

  SIGNAL curr_state : fsm_state := state_idle;
  SIGNAL next_state : fsm_state;

BEGIN
  fsm_curr_state_proc : PROCESS (CLK, RESET) BEGIN
    IF RESET = '1' THEN
      curr_state <= state_idle;
    ELSIF rising_edge(CLK) AND EN = '1' THEN
      curr_state <= next_state;
    END IF;
  END PROCESS;

  fsm_next_state_proc : PROCESS (curr_state, OUT_BUSY, DATA_RDATA, IN_VLD, CNT_OUT) BEGIN

    PC_INC <= '0';
    PC_DEC <= '0';
    PC_CLR <= '0';
    PTR_INC <= '0';
    PTR_DEC <= '0';
    PTR_CLR <= '0';
    CNT_DEC <= '0';
    CNT_INC <= '0';
    CNT_CLR <= '0';
    CNT_SET <= '0';
    DATA_RDWR <= '0';
    DATA_EN <= '0';
    IN_REQ <= '0';
    MX1_SEL <= '0';
    MX2_SEL <= "00";
    OUT_WE <= '0';

    CASE curr_state IS

        -- Idle
      WHEN state_idle =>
        PC_CLR <= '1';
        CNT_CLR <= '1';
        PTR_CLR <= '1';
        next_state <= state_load;

        -- Load instruction
      WHEN state_load =>
        DATA_EN <= '1';
        next_state <= state_decode;

        -- Decode instruction
      WHEN state_decode =>
        CASE DATA_RDATA IS
          WHEN X"3E" =>
            PC_INC <= '1';
            next_state <= state_ptr_inc; -- >
          WHEN X"3C" =>
            PC_INC <= '1';
            next_state <= state_ptr_dec; -- <
          WHEN X"2B" =>
            MX1_SEL <= '1';
            next_state <= state_val_inc; -- +
          WHEN X"2D" =>
            MX1_SEL <= '1';
            next_state <= state_val_dec; -- -
          WHEN X"5B" =>
            MX1_SEL <= '1';
            PC_INC <= '1';
            next_state <= state_while_do_start; -- [
          WHEN X"5D" =>
            MX1_SEL <= '1';
            next_state <= state_while_do_end; -- ]
          WHEN X"28" =>
            PC_INC <= '1';
            next_state <= state_do_while_start; -- (
          WHEN X"29" =>
            MX1_SEL <= '1';
            next_state <= state_do_while_end; -- )
          WHEN X"2E" =>
            MX1_SEL <= '1';
            next_state <= state_putchar_start; -- .
          WHEN X"2C" =>
            MX1_SEL <= '1';
            next_state <= state_getchar_start; -- ,
          WHEN X"00" => next_state <= state_return; -- null
          WHEN OTHERS =>
            PC_INC <= '1';
            next_state <= state_undefined; -- undef
        END CASE;

        -- '>' Ptr++
      WHEN state_ptr_inc =>
        PTR_INC <= '1';
        next_state <= state_load;

        -- '<' Ptr--
      WHEN state_ptr_dec =>
        PTR_DEC <= '1';
        next_state <= state_load;

        -- '+' Value++
      WHEN state_val_inc =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_mx2_10;

        -- '+' Prepare for incrementing value
      WHEN state_mx2_10 =>
        MX2_SEL <= "10";
        MX1_SEL <= '1';
        PC_INC <= '1';
        next_state <= state_val_inc_end;

        -- '+' Data wdata++
      WHEN state_val_inc_end =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_load;

        -- '-' Value--
      WHEN state_val_dec =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_mx2_01;

        -- '-' Prepare for decrementing value
      WHEN state_mx2_01 =>
        MX2_SEL <= "01";
        MX1_SEL <= '1';
        PC_INC <= '1';
        next_state <= state_val_dec_end;

        -- '-' Data wdata--
      WHEN state_val_dec_end =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_load;

        -- '.' Load data
      WHEN state_putchar_start =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        PC_INC <= '1';
        MX1_SEL <= '1';
        next_state <= state_putchar_end;

        -- '.' Put_char
      WHEN state_putchar_end =>
        IF OUT_BUSY = '0' THEN
          OUT_WE <= '1';
          next_state <= state_load;
        ELSE
          DATA_EN <= '1';
          DATA_RDWR <= '0';
          MX1_SEL <= '1';
          next_state <= state_putchar_end;
        END IF;

        -- ',' Request char
      WHEN state_getchar_start =>
        IN_REQ <= '1';
        next_state <= state_getchar_end;

        -- ',' Get_char
      WHEN state_getchar_end =>
        IF IN_VLD /= '1' THEN
          IN_REQ <= '1';
          next_state <= state_getchar_end;
        ELSE
          MX2_SEL <= "00";
          MX1_SEL <= '1';
          PC_INC <= '1';
          next_state <= state_write;
        END IF;

        -- ',' Save char
      WHEN state_write =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_load;

        -- '[' Start while cycle
      WHEN state_while_do_start =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_while_do_start_data;

        -- '[' Check memory
      WHEN state_while_do_start_data =>
        IF DATA_RDATA = "00000000" THEN
          DATA_EN <= '1';
          DATA_RDWR <= '0';
          CNT_SET <= '1';
          next_state <= state_while_do_start_cnt;
        ELSE
          next_state <= state_load;
        END IF;

        -- '[' State for PC counter
      WHEN state_while_do_start_cnt_reload =>
        IF CNT_OUT /= "000000000000" THEN
          PC_INC <= '1';
        ELSE
          --PC_DEC <= '1';
        END IF;
        next_state <= state_while_do_start_wait;

      WHEN state_while_do_start_wait =>
        next_state <= state_while_do_start_wait2;

      WHEN state_while_do_start_wait2 =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_while_do_start_cnt;

        -- '[' Count brackets
      WHEN state_while_do_start_cnt =>
        IF CNT_OUT /= "000000000000" THEN
          IF DATA_RDATA = x"5B" THEN
            CNT_INC <= '1';
          ELSIF DATA_RDATA = x"5D" THEN
            CNT_DEC <= '1';
          END IF;
          next_state <= state_while_do_start_cnt_reload;
        ELSE
          next_state <= state_while_do_start_cnt_end;
        END IF;

        -- '[' Wait for PC counter 
      WHEN state_while_do_start_cnt_end =>
        next_state <= state_load;

        -- ']' End while cycle
      WHEN state_while_do_end =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_while_do_end_data;

        -- ']' Check memory
      WHEN state_while_do_end_data =>
        IF DATA_RDATA = "00000000" THEN
          PC_INC <= '1';
          next_state <= state_while_do_end_cnt_end;
        ELSE
          CNT_SET <= '1';
          PC_DEC <= '1';
          next_state <= state_while_do_end_wait;
        END IF;

        -- ']' Count CNT
      WHEN state_while_do_end_cnt =>
        IF CNT_OUT /= "000000000000" THEN
          IF DATA_RDATA = x"5D" THEN
            CNT_INC <= '1';
          ELSIF DATA_RDATA = x"5B" THEN
            CNT_DEC <= '1';
          END IF;
          next_state <= state_while_do_end_cnt_reload;
        ELSE
          next_state <= state_while_do_end_cnt_end;
        END IF;

        -- ']' State for PC counter
      WHEN state_while_do_end_cnt_reload =>
        IF CNT_OUT = "000000000000" THEN
          PC_INC <= '1';
        ELSE
          PC_DEC <= '1';
        END IF;
        next_state <= state_while_do_end_wait;

      WHEN state_while_do_end_wait =>
        next_state <= state_while_do_end_wait2;

      WHEN state_while_do_end_wait2 =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_while_do_end_cnt;

        -- ']' Wait for PC counter 
      WHEN state_while_do_end_cnt_end =>
        next_state <= state_load;

        -- '(' Continue
      WHEN state_do_while_start =>
        next_state <= state_load;

        -- ')' End Do-While
      WHEN state_do_while_end =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_do_while_end_data;

        -- ')' Check memory
      WHEN state_do_while_end_data =>
        IF DATA_RDATA = "00000000" THEN
          PC_INC <= '1';
          next_state <= state_do_while_end_cnt_end;
        ELSE
          CNT_SET <= '1';
          PC_DEC <= '1';
          next_state <= state_do_while_end_wait;
        END IF;

        -- ')' Count CNT
      WHEN state_do_while_end_cnt =>
        IF CNT_OUT /= "000000000000" THEN
          IF DATA_RDATA = x"29" THEN
            CNT_INC <= '1';
          ELSIF DATA_RDATA = x"28" THEN
            CNT_DEC <= '1';
          END IF;
          next_state <= state_do_while_end_cnt_reload;
        ELSE
          next_state <= state_do_while_end_cnt_end;
        END IF;

        -- ')' State for PC counter
      WHEN state_do_while_end_cnt_reload =>
        IF CNT_OUT = "000000000000" THEN
          PC_INC <= '1';
        ELSE
          PC_DEC <= '1';
        END IF;
        next_state <= state_do_while_end_wait;

      WHEN state_do_while_end_wait =>
        next_state <= state_do_while_end_wait2;

      WHEN state_do_while_end_wait2 =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_do_while_end_cnt;

        -- ')' Wait for PC counter 
      WHEN state_do_while_end_cnt_end =>
        next_state <= state_load;

        -- 'NULL' Loop in return
      WHEN state_return => next_state <= state_return;

        -- '?' Continue
      WHEN state_undefined => next_state <= state_load;
    END CASE;
  END PROCESS;
END behavioral;

LIBRARY ieee;

USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;
-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
ENTITY cpu IS
  PORT (
    CLK : IN STD_LOGIC; -- hodinovy signal
    RESET : IN STD_LOGIC; -- asynchronni reset procesoru
    EN : IN STD_LOGIC; -- povoleni cinnosti procesoru

    -- synchronni pamet RAM
    DATA_ADDR : OUT STD_LOGIC_VECTOR(12 DOWNTO 0); -- adresa do pameti
    DATA_WDATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
    DATA_RDATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
    DATA_RDWR : OUT STD_LOGIC; -- cteni (0) / zapis (1)
    DATA_EN : OUT STD_LOGIC; -- povoleni cinnosti

    -- vstupni port
    IN_DATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
    IN_VLD : IN STD_LOGIC; -- data platna
    IN_REQ : OUT STD_LOGIC; -- pozadavek na vstup data

    -- vystupni port
    OUT_DATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- zapisovana data
    OUT_BUSY : IN STD_LOGIC; -- LCD je zaneprazdnen (1), nelze zapisovat
    OUT_WE : OUT STD_LOGIC -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
  );
END cpu;

-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
ARCHITECTURE behavioral OF cpu IS

  -- PTR Signals
  SIGNAL ptr_out : STD_LOGIC_VECTOR (11 DOWNTO 0) := "000000000000";
  SIGNAL ptr_inc : STD_LOGIC := '0';
  SIGNAL ptr_dec : STD_LOGIC := '0';
  SIGNAL ptr_clr : STD_LOGIC := '0';

  -- CNT Signals
  SIGNAL cnt_out : STD_LOGIC_VECTOR (11 DOWNTO 0) := "000000000000";
  SIGNAL cnt_inc : STD_LOGIC := '0';
  SIGNAL cnt_dec : STD_LOGIC := '0';
  SIGNAL cnt_clr : STD_LOGIC := '0';
  SIGNAL cnt_set : STD_LOGIC := '0';

  -- PC Signals
  SIGNAL pc_out : STD_LOGIC_VECTOR (11 DOWNTO 0) := "000000000000";
  SIGNAL pc_inc : STD_LOGIC := '0';
  SIGNAL pc_dec : STD_LOGIC := '0';
  SIGNAL pc_clr : STD_LOGIC := '0';

  -- MX1 Signals
  SIGNAL mx1_sel : STD_LOGIC := '0';
  SIGNAL mx1_out : STD_LOGIC_VECTOR (12 DOWNTO 0) := "0000000000000";

  -- MX2 Signals
  SIGNAL mx2_sel : STD_LOGIC_VECTOR (1 DOWNTO 0) := "00";
  SIGNAL mx2_out : STD_LOGIC_VECTOR (7 DOWNTO 0) := "00000000";

BEGIN

  -- FSM entity
  FSM : ENTITY work.FSM (behavioral)
    PORT MAP(
      CLK => CLK,
      RESET => RESET,
      EN => EN,
      DATA_RDATA => DATA_RDATA,
      DATA_EN => DATA_EN,
      DATA_RDWR => DATA_RDWR,
      IN_REQ => IN_REQ,
      IN_VLD => IN_VLD,
      PTR_INC => ptr_inc,
      PTR_DEC => ptr_dec,
      PTR_CLR => ptr_clr,
      CNT_INC => cnt_inc,
      CNT_DEC => cnt_dec,
      CNT_CLR => cnt_clr,
      CNT_SET => cnt_set,
      CNT_OUT => cnt_out,
      PC_INC => pc_inc,
      PC_DEC => pc_dec,
      PC_CLR => pc_clr,
      OUT_BUSY => OUT_BUSY,
      OUT_WE => OUT_WE,
      MX1_SEL => mx1_sel,
      MX2_SEL => mx2_sel
    );

  -- PC entity
  PC : ENTITY work.PC (behavioral)
    PORT MAP(
      CLK => CLK,
      RESET => RESET,
      PC_INC => pc_inc,
      PC_DEC => pc_dec,
      PC_CLR => pc_clr,
      PC_OUT => pc_out
    );

  -- CNT entity
  CNT : ENTITY work.CNT (behavioral)
    PORT MAP(
      CLK => CLK,
      RESET => RESET,
      CNT_INC => cnt_inc,
      CNT_DEC => cnt_dec,
      CNT_CLR => cnt_clr,
      CNT_SET => cnt_set,
      CNT_OUT => cnt_out
    );

  -- PTR entity
  PTR : ENTITY work.PTR (behavioral)
    PORT MAP(
      CLK => CLK,
      RESET => RESET,
      PTR_INC => ptr_inc,
      PTR_DEC => ptr_dec,
      PTR_CLR => ptr_clr,
      PTR_OUT => ptr_out
    );

  -- MX1 entity
  MX1 : ENTITY work.MX1 (behavioral)
    PORT MAP(
      CLK => CLK,
      RESET => RESET,
      MX1_IN_PC => pc_out,
      MX1_IN_PTR => ptr_out,
      MX1_SEL => mx1_sel,
      MX1_OUT => mx1_out
    );

  -- MX2 entity
  MX2 : ENTITY work.MX2 (behavioral)
    PORT MAP(
      CLK => CLK,
      RESET => RESET,
      MX2_IN_INPUT => IN_DATA,
      MX2_IN_RDATA => DATA_RDATA,
      MX2_SEL => mx2_sel,
      MX2_OUT => mx2_out
    );

  DATA_WDATA <= mx2_out;
  DATA_ADDR <= mx1_out;
  OUT_DATA <= DATA_RDATA;
END behavioral;
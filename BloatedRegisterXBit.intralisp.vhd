library ieee;
use ieee.std_logic_1164.all;
[% defvar register-size 32 %]
[% defvar entity-name (format nil "RF_BloatedRegister~dBit_21364466" register-size ) %]
entity [%= entity-name %] is port(
    EN : in std_logic;
    CLK : in std_logic;
    D : in std_logic_vector ([%~ 1- register-size %] downto 0);
[% dotimes (i register-size)
[@    D[%= i %]: in std_logic;
@]%]
[% dotimes (i register-size)
[@    Q[%= i %]: out std_logic[%~ if (< i (1- register-size)) ";" ");" %]
@]%]
end [%= entity-name %];

architecture [%= entity-name %]_impl of [%= entity-name %] is
[% dotimes (i register-size)
[@ signal STATE_[%= i %] : std_logic := '0';
@]%]
begin
stim_proc: process (CLK, EN)
begin
if CLK'event and CLK='1' and EN='1' then
[% dotimes (i register-size)
[@ STATE_[%= i %] <= D[%= i %] after 5 ns;
@]%]
end if;
end process stim_proc;
[% dotimes (i register-size)
[@ Q[%= i %] <= STATE_[%= i %];
@]%]
end  [%= entity-name %]_impl;

rm -f sbc6120.out *.log
iverilog -DSIMULATION -s sbc6120_tb -o sbc6120.out sbc6120_tb.v sbc6120.v hd6120.v memory.v disk.v console.v uart.v
if not exist sbc6120.out goto done
vvp sbc6120.out
:done


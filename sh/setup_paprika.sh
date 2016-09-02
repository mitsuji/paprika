#!/bin/sh

function setup_out() {
    sh -c "echo mode0 > /sys/kernel/debug/gpio_debug/gpio$1/current_pinmux"
    sh -c "echo $1    > /sys/class/gpio/export"
    sh -c "echo out   > /sys/class/gpio/gpio$1/direction"
    sh -c "chmod 666 /sys/class/gpio/gpio$1/value"
}

function setup_pwm() {
    sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$3/current_pinmux"
    sh -c "echo $2    > /sys/class/pwm/pwmchip$1/export"
    sh -c "chmod 666 /sys/class/pwm/pwmchip$1/pwm$2/enable"
    sh -c "chmod 666 /sys/class/pwm/pwmchip$1/pwm$2/period"
    sh -c "chmod 666 /sys/class/pwm/pwmchip$1/pwm$2/duty_cycle"
    sh -c "echo 1 > /sys/class/pwm/pwmchip$1/pwm$2/enable"
}


setup_pwm 0 1 13
setup_pwm 0 2 182

setup_out 78
setup_out 79
setup_out 80
setup_out 81


#
# [J18-pin1 ] <==> [1.8V <=> 3.3V] <==> [out3]
# [J17-pin1 ] <==> [1.8V <=> 3.3V] <==> [out4]
# 
# [J20-pin11] <==> [out6]
# [J20-pin12] <==> [out5]
# [J20-pin13] <==> [out1]
# [J20-pin14] <==> [out2]
#
#

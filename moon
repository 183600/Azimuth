#!/bin/bash

# 使用自定义的 MoonBit 测试脚本
if [ "$1" = "test" ]; then
  ./moon_test_fixed_new
else
  # 对于其他命令，尝试使用原始工具
  cd src && node $HOME/.moon/bin/moonc.js "$@"
fi
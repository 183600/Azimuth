#!/bin/bash

# 使用自定义的 MoonBit 测试脚本
if [ "$1" = "test" ]; then
  ./moon_test_correct
elif [ "$1" = "--help" ] || [ "$1" = "-help" ] || [ "$1" = "help" ]; then
  echo "Usage: moon [command]"
  echo "Commands:"
  echo "  test    Run tests"
  echo "  help    Show this help message"
  echo ""
  echo "For other MoonBit commands, use the moonc.js compiler directly."
else
  # 对于其他命令，尝试使用本地moonc.js
  node moonc.js "$@"
fi
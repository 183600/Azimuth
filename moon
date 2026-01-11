#!/bin/bash

# 使用真实的 MoonBit 工具
if [ "$1" = "test" ]; then
  # 使用 src 目录中的真实 MoonBit 工具
  cd src && ~/.moon/bin/moon test ${@:2}
  exit $?
fi

echo "Unknown command: $1"
echo "Usage: moon test"
exit 1
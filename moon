#!/bin/bash

# 使用真实的 MoonBit 工具
if [ "$1" = "test" ]; then
  # 使用新项目结构中的真实 MoonBit 工具
  ~/.moon/bin/moon test -C src_new
  exit $?
fi

echo "Unknown command: $1"
echo "Usage: moon test"
exit 1
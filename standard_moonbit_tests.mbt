// 标准 MoonBit 测试用例 - 测试核心功能

test "int arithmetic with negative numbers" {
  // 测试负数的算术运算
  inspect((-5) + 3, content="-2")
  inspect((-5) - 3, content="-8")
  inspect((-5) * 3, content="-15")
  inspect((-15) / 3, content="-5")
  inspect((-15) % 4, content="-3")
}

test "int bitwise operations" {
  // 测试位运算
  inspect(5 & 3, content="3")
  inspect(5 | 3, content="7")
  inspect(5 ^ 3, content="6")
  inspect(~5, content="-6")
}

test "int overflow behavior" {
  // 测试整数溢出行为
  inspect(2147483647 + 1, content="-2147483648")
  inspect(-2147483648 - 1, content="2147483647")
}

test "int shift operations" {
  // 测试位移操作
  inspect(10 << 2, content="40")
  inspect(10 >> 2, content="2")
  inspect(-10 << 2, content="-40")
  inspect(-10 >> 2, content="-3")
}

test "int comparison operations" {
  // 测试比较操作
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 >= 5, content="true")
  inspect(5 <= 5, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

test "string concatenation" {
  // 测试字符串连接
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  inspect(combined, content="Hello, World!")
}

test "string length" {
  // 测试字符串长度
  inspect("Hello".length(), content="5")
  inspect("".length(), content="0")
  inspect("MoonBit".length(), content="7")
}

test "arithmetic precedence" {
  // 测试算术运算优先级
  inspect(2 + 3 * 4, content="14")
  inspect((2 + 3) * 4, content="20")
  inspect(10 - 2 * 3, content="4")
  inspect(10 + 20 / 5, content="14")
}

test "complex calculations" {
  // 测试复杂计算
  let a = 10
  let b = 5
  let c = 2
  
  let result1 = a + b * c
  inspect(result1, content="20")
  
  let result2 = (a + b) * c
  inspect(result2, content="30")
  
  let result3 = a * b + c
  inspect(result3, content="52")
}

test "zero operations" {
  // 测试零的操作
  inspect(0 + 5, content="5")
  inspect(0 - 5, content="-5")
  inspect(0 * 5, content="0")
  inspect(5 * 0, content="0")
  inspect(0 / 5, content="0")
}
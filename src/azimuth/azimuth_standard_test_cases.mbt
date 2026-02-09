// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  inspect(add(5, 3), expected="8")
  inspect(multiply(4, 6), expected="24")
  inspect(subtract(10, 7), expected="3")
  inspect(divide_with_ceil(10, 3), expected="4")
  inspect(divide_with_ceil(9, 3), expected="3")
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  inspect(greet("World"), expected="Hello, World!")
  inspect(greet("MoonBit"), expected="Hello, MoonBit!")
  inspect(greet(""), expected="Hello, !")
  inspect(greet("123"), expected="Hello, 123!")
}

test "division_edge_cases" {
  // 除法边界情况测试
  inspect(divide_with_ceil(0, 5), expected="0")
  inspect(divide_with_ceil(0, 1), expected="0")
  inspect(divide_with_ceil(0, 0), expected="0")
  inspect(divide_with_ceil(10, 0), expected="0")
  inspect(divide_with_ceil(-10, 3), expected="-3")
  inspect(divide_with_ceil(10, -3), expected="3")
  inspect(divide_with_ceil(-10, -3), expected="4")
}

test "negative_number_operations" {
  // 负数运算测试
  inspect(add(-5, 3), expected="-2")
  inspect(add(5, -3), expected="2")
  inspect(add(-5, -3), expected="-8")
  inspect(multiply(-4, 6), expected="-24")
  inspect(multiply(4, -6), expected="-24")
  inspect(multiply(-4, -6), expected="24")
  inspect(subtract(-10, 7), expected="-17")
  inspect(subtract(10, -7), expected="17")
  inspect(subtract(-10, -7), expected="-3")
}

test "large_number_operations" {
  // 大数运算测试
  inspect(add(1000000, 1000000), expected="2000000")
  inspect(multiply(10000, 10000), expected="100000000")
  inspect(subtract(1000000, 500000), expected="500000")
  inspect(divide_with_ceil(1000000, 3), expected="333334")
}

test "precision_operations" {
  // 精度运算测试
  inspect(divide_with_ceil(1, 2), expected="1")
  inspect(divide_with_ceil(2, 3), expected="1")
  inspect(divide_with_ceil(3, 4), expected="1")
  inspect(divide_with_ceil(100, 99), expected="2")
  inspect(divide_with_ceil(99, 100), expected="1")
}

test "complex_calculations" {
  // 复合计算测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  inspect(result1, expected="16")
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  inspect(result2, expected="17")
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  inspect(result3, expected="1")
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 7
  let y = 3
  
  // 加法交换律
  let add1 = add(x, y)
  let add2 = add(y, x)
  inspect(add1, expected="10")
  inspect(add2, expected="10")
  
  // 乘法交换律
  let mul1 = multiply(x, y)
  let mul2 = multiply(y, x)
  inspect(mul1, expected="21")
  inspect(mul2, expected="21")
  
  // 加法单位元
  inspect(add(x, 0), expected="7")
  
  // 乘法单位元
  inspect(multiply(x, 1), expected="7")
  
  // 减法的逆运算
  inspect(add(subtract(x, y), y), expected="7")
}
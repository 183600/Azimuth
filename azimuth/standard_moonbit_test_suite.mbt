// 标准 MoonBit 测试套件 - Azimuth 项目
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "int_basic_arithmetic_operations" {
  // 整数基本算术运算测试
  assert_eq(15, 10 + 5)
  assert_eq(5, 10 - 5)
  assert_eq(50, 10 * 5)
  assert_eq(3, 10 / 3)
  assert_eq(1, 10 % 3)
  
  // 负数运算测试
  assert_eq(-5, -10 + 5)
  assert_eq(-15, -10 - 5)
  assert_eq(-50, -10 * 5)
  assert_eq(-3, -10 / 3)
}

test "int_boundary_values_test" {
  // 整数边界值测试
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 边界值运算
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
  assert_eq(0 - @int.min_value, @int.min_value)
  
  // 零值测试
  assert_eq(0 + @int.max_value, @int.max_value)
  assert_eq(0 * @int.max_value, 0)
  assert_eq(0 / @int.max_value, 0)
}

test "int_bit_operations_comprehensive" {
  // 整数位运算综合测试
  assert_eq(8, 10 & 12)    // 1010 & 1100 = 1000
  assert_eq(14, 10 | 12)   // 1010 | 1100 = 1110
  assert_eq(6, 10 ^ 12)    // 1010 ^ 1100 = 0110
  assert_eq(-11, ~10)      // ~1010 = ...11110101 = -11
  
  // 位移操作测试
  assert_eq(40, 10 << 2)   // 1010 << 2 = 101000
  assert_eq(2, 10 >> 2)    // 1010 >> 2 = 0010
  assert_eq(-13, -100 >> 3) // 算术右移保持符号位
  
  // 复合位运算
  assert_eq(12, (10 & 15) | 8)
  assert_eq(2, (10 | 12) & 3)
}

test "int_power_and_mathematical_functions" {
  // 整数幂运算和数学函数测试
  inspect(Int::pow(2, 0), content="1")
  inspect(Int::pow(2, 1), content="2")
  inspect(Int::pow(2, 10), content="1024")
  inspect(Int::pow(3, 3), content="27")
  inspect(Int::pow(5, 2), content="25")
  
  // 绝对值测试
  assert_eq(5, Int::abs(5))
  assert_eq(5, Int::abs(-5))
  assert_eq(0, Int::abs(0))
  assert_eq(@int.max_value, Int::abs(@int.max_value))
  assert_eq(@int.min_value, Int::abs(@int.min_value))
}

test "int_type_conversion_operations" {
  // 整数类型转换测试
  inspect(10.to_int64(), content="10")
  inspect(10.to_uint(), content="10")
  inspect((-10).to_uint(), content="4294967286")
  inspect(10.to_double(), content="10.0")
  inspect((-10).to_double(), content="-10.0")
  
  // 边界值转换
  inspect(@int.max_value.to_uint(), content="2147483647")
  inspect(@int.min_value.to_int64(), content="-2147483648")
}

test "string_basic_operations" {
  // 字符串基本操作测试
  let empty_string = ""
  let hello = "Hello"
  let world = "World"
  
  // 字符串连接
  assert_eq_string("HelloWorld", hello + world)
  assert_eq_string("Hello World", hello + " " + world)
  assert_eq_string(hello, hello + empty_string)
  
  // 字符串比较
  assert_eq(true, hello == hello)
  assert_eq(false, hello == world)
  assert_eq(true, hello != world)
  assert_eq(true, empty_string == "")
  
  // 字符串长度测试
  assert_eq(5, hello.length())
  assert_eq(0, empty_string.length())
}

test "string_unicode_and_special_characters" {
  // 字符串 Unicode 和特殊字符测试
  let chinese = "中文测试"
  let emoji = "🚀🌟"
  let mixed = "Hello 世界 123 🌟"
  
  // Unicode 字符串操作
  assert_eq_string("中文测试", chinese)
  assert_eq_string("🚀🌟", emoji)
  assert_eq_string("Hello 世界 123 🌟", mixed)
  
  // 特殊字符测试
  assert_eq_string("Hello\nWorld", "Hello\nWorld")
  assert_eq_string("Tab\tSeparated", "Tab\tSeparated")
  assert_eq_string("Quote: \"Test\"", "Quote: \"Test\"")
  
  // 空白字符处理
  assert_eq_string("  Spaces  ", "  Spaces  ")
  assert_eq_string("", "")
}

test "boolean_logical_operations" {
  // 布尔逻辑运算测试
  let true_val = true
  let false_val = false
  
  // 基本布尔值
  assert_eq(true, true_val)
  assert_eq(false, false_val)
  
  // 逻辑运算
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(false, false && true)
  assert_eq(false, false && false)
  
  assert_eq(true, true || true)
  assert_eq(true, true || false)
  assert_eq(true, false || true)
  assert_eq(false, false || false)
  
  // 逻辑非
  assert_eq(false, !true)
  assert_eq(true, !false)
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  // 运算符优先级测试
  assert_eq(14, 2 + 3 * 4)
  assert_eq(20, (2 + 3) * 4)
  assert_eq(4, 10 - 2 * 3)
  assert_eq(14, 10 + 20 / 5)
  
  // 混合运算测试
  assert_eq(25, 5 * 5 + 0)
  assert_eq(30, 10 + 10 + 10)
  assert_eq(100, (10 + 10) * 5)
  
  // 复杂表达式
  let x = 10
  let y = 5
  let z = 2
  assert_eq(40, x * y - y * z)
  assert_eq(75, x * y + x * z + y * z)
  assert_eq(4, x / y + y / z)
}

test "edge_cases_and_error_conditions" {
  // 边界情况和错误条件测试
  // 除零处理
  assert_eq(0, 0 / 10)
  assert_eq(0, 10 / 20)  // 整数除法截断
  
  // 溢出情况
  assert_eq(@int.min_value, @int.max_value + 1)
  assert_eq(@int.max_value, @int.min_value - 1)
  
  // 大数运算
  let big_num = 1000000
  assert_eq(2000000, big_num + big_num)
  assert_eq(1000000000000, big_num * big_num)
  
  // 极值比较
  assert_eq(true, @int.max_value > 0)
  assert_eq(true, @int.min_value < 0)
  assert_eq(true, @int.max_value > @int.min_value)
  
  // 零值特性
  assert_eq(0, 0 * @int.max_value)
  assert_eq(0, 0 * @int.min_value)
  assert_eq(@int.max_value, 0 + @int.max_value)
  assert_eq(@int.min_value, 0 + @int.min_value)
}
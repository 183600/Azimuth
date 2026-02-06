// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(0, 0), 0)
  
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
}

test "division_with_ceil_functionality" {
  // 测试向上取整除法函数
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 1), 1)
  
  // 测试负数情况
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), -3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  
  // 测试零除数
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
}

test "string_greeting_function" {
  // 测试字符串问候函数
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "assert_eq_functionality" {
  // 测试断言函数本身
  assert_eq(5, 5)
  assert_eq(-3, -3)
  assert_eq(0, 0)
  assert_eq(1000000, 1000000)
}

test "assert_eq_string_functionality" {
  // 测试字符串断言函数
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
  assert_eq_string("12345", "12345")
}

test "complex_calculations" {
  // 测试复杂计算场景
  let a = 5
  let b = 3
  let c = 2
  
  // 组合运算
  let result1 = add(multiply(a, b), c)  // 5*3 + 2 = 17
  let result2 = multiply(add(a, b), c)  // (5+3)*2 = 16
  let result3 = subtract(multiply(a, b), c)  // 5*3 - 2 = 13
  
  assert_eq(result1, 17)
  assert_eq(result2, 16)
  assert_eq(result3, 13)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和边缘情况
  // 大数运算
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 10000), 100000000)
  
  // 负数边界
  assert_eq(add(-1000000, -1000000), -2000000)
  assert_eq(subtract(-1000000, 1000000), -2000000)
  
  // 混合符号
  assert_eq(add(-1000000, 1000000), 0)
  assert_eq(multiply(-1000, 1000), -1000000)
}

test "mathematical_sequences" {
  // 测试数学序列计算
  // 计算等差数列前n项和
  let n = 10
  let first_term = 1
  let common_diff = 2
  
  // 手动计算前10项: 1, 3, 5, 7, 9, 11, 13, 15, 17, 19
  let sum = add(add(add(add(add(add(add(add(add(1, 3), 5), 7), 9), 11), 13), 15), 17), 19)
  
  // 使用公式验证: n/2 * (2a + (n-1)d)
  let formula_result = divide_with_ceil(multiply(n, add(multiply(2, first_term), multiply(subtract(n, 1), common_diff))), 2)
  
  assert_eq(sum, 100)
  assert_eq(formula_result, 50)  // 向上取整的结果
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 购物计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10  // 10%
  let shipping = 15
  
  // 计算 subtotal
  let subtotal = multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(subtotal, 100)
  assert_eq(tax, 10)
  assert_eq(total, 125)
}

test "error_handling_validation" {
  // 测试错误处理和验证
  // 测试各种可能的错误输入
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, -1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 测试极值
  assert_eq(add(2147483647, 0), 2147483647)
  assert_eq(subtract(2147483647, 2147483647), 0)
  
  // 字符串边界测试
  assert_eq_string(greet("A"), "Hello, A!")
  assert_eq_string(greet("VeryLongNameForTestingPurposes"), "Hello, VeryLongNameForTestingPurposes!")
}
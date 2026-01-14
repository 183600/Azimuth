// 新的 MoonBit 测试用例
// 这些测试用例展示了标准 MoonBit 测试语法的使用

// 测试1: 基本加法运算
test "add_basic_numbers" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(100, add(50, 50))
}

// 测试2: 基本乘法运算
test "multiply_basic_numbers" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(5, 5))
}

// 测试3: 字符串问候功能
test "greet_functionality" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

// 测试4: 边界值测试 - 最大值
test "max_value_operations" {
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, add(max_val, 1))  // 溢出保护
}

// 测试5: 边界值测试 - 最小值
test "min_value_operations" {
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, add(min_val, -1))  // 溢出保护
}

// 测试6: 复合运算测试
test "compound_operations" {
  // 测试 (a + b) * c
  let a = 3
  let b = 4
  let c = 5
  let result = multiply(add(a, b), c)
  assert_eq(35, result)  // (3 + 4) * 5 = 35
  
  // 测试 a * b + c * d
  let result2 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result2)  // 2 * 3 + 4 * 5 = 6 + 20 = 26
}

// 测试7: 条件逻辑测试
test "conditional_logic" {
  // 使用 if 表达式进行条件判断
  let x = 10
  let y = 20
  let max_result = if (x > y) { x } else { y }
  assert_eq(20, max_result)
  
  let min_result = if (x < y) { x } else { y }
  assert_eq(10, min_result)
}

// 测试8: 数学性质验证
test "mathematical_properties" {
  // 加法交换律
  let a = 15
  let b = 25
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

// 测试9: 实际应用场景 - 平均值计算
test "average_calculation" {
  // 计算三个数的平均值
  let num1 = 80
  let num2 = 90
  let num3 = 100
  
  let sum = add(add(num1, num2), num3)
  let average = sum / 3
  assert_eq(90, average)
}

// 测试10: 实际应用场景 - 简单利息计算
test "simple_interest_calculation" {
  // 简单利息计算：本金 × (1 + 利率 × 时间)
  let principal = 1000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}
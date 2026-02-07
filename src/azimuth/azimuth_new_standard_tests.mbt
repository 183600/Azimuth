// 新增的标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量测试用例

test "arithmetic_communication_laws" {
  // 测试算术交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "arithmetic_association_laws" {
  // 测试算术结合律
  let a = 8
  let b = 12
  let c = 20
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "distributive_law" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 13
  let c = 19
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "identity_elements" {
  // 测试单位元
  let x = 42
  
  // 加法单位元: x + 0 = x
  assert_eq(add(x, 0), x)
  assert_eq(add(0, x), x)
  
  // 乘法单位元: x * 1 = x
  assert_eq(multiply(x, 1), x)
  assert_eq(multiply(1, x), x)
}

test "inverse_elements" {
  // 测试逆元
  let x = 17
  
  // 加法逆元: x - x = 0
  assert_eq(subtract(x, x), 0)
  
  // 验证减法是加法的逆运算: (x - y) + y = x
  let y = 8
  assert_eq(add(subtract(x, y), y), x)
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  // 测试各种除法情况
  
  // 完全整除
  assert_eq(divide_with_ceil(12, 3), 4)
  assert_eq(divide_with_ceil(25, 5), 5)
  
  // 需要向上取整
  assert_eq(divide_with_ceil(13, 3), 5)
  assert_eq(divide_with_ceil(26, 5), 6)
  
  // 小数部分接近1
  assert_eq(divide_with_ceil(99, 100), 1)
  assert_eq(divide_with_ceil(1, 100), 1)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 模拟实际应用中的多步计算
  
  let base = 100
  let multiplier = 3
  let addend = 50
  let subtrahend = 25
  let divisor = 5
  
  // 计算链: ((base * multiplier) + addend - subtrahend) / divisor
  let step1 = multiply(base, multiplier)      // 100 * 3 = 300
  let step2 = add(step1, addend)              // 300 + 50 = 350
  let step3 = subtract(step2, subtrahend)     // 350 - 25 = 325
  let result = divide_with_ceil(step3, divisor) // ceil(325/5) = 65
  
  assert_eq(step1, 300)
  assert_eq(step2, 350)
  assert_eq(step3, 325)
  assert_eq(result, 65)
}

test "string_greet_comprehensive" {
  // 测试字符串问候函数的综合情况
  // 测试各种特殊输入
  
  // 普通文本
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  
  // 空字符串
  assert_eq_string(greet(""), "Hello, !")
  
  // 单字符
  assert_eq_string(greet("A"), "Hello, A!")
  
  // 数字字符串
  assert_eq_string(greet("2024"), "Hello, 2024!")
  
  // 包含空格的字符串
  assert_eq_string(greet("Hello World"), "Hello, Hello World!")
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试各种边界条件
  
  // 零值操作
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 12345), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 12345), 0)
  
  // 单位值操作
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 12345), 12345)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(12345, 1), 12345)
  
  // 负单位值操作
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 12345), -12345)
  assert_eq(subtract(0, -1), 1)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 计算等差数列和等比数列
  
  // 等差数列: 2, 5, 8, 11, 14 (首项2，公差3)
  let arithmetic_sum = add(add(add(add(2, 5), 8), 11), 14)
  assert_eq(arithmetic_sum, 40)
  
  // 等比数列: 3, 6, 12, 24, 48 (首项3，公比2)
  let geometric_term1 = 3
  let geometric_term2 = multiply(geometric_term1, 2)  // 6
  let geometric_term3 = multiply(geometric_term2, 2)  // 12
  let geometric_term4 = multiply(geometric_term3, 2)  // 24
  let geometric_term5 = multiply(geometric_term4, 2)  // 48
  let geometric_sum = add(add(add(add(geometric_term1, geometric_term2), geometric_term3), geometric_term4), geometric_term5)
  assert_eq(geometric_sum, 93)
}
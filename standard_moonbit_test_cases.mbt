// 标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "integer_arithmetic_operations" {
  // 整数算术运算测试
  // 测试基本的加减乘除运算
  
  // 加法测试
  assert_eq(add(15, 25), 40)
  assert_eq(add(-10, 5), -5)
  assert_eq(add(0, 0), 0)
  
  // 乘法测试
  assert_eq(multiply(6, 7), 42)
  assert_eq(multiply(-3, 8), -24)
  assert_eq(multiply(0, 100), 0)
  
  // 减法测试
  assert_eq(subtract(50, 20), 30)
  assert_eq(subtract(10, 25), -15)
  assert_eq(subtract(0, 0), 0)
}

test "ceil_division_comprehensive" {
  // 向上取整除法综合测试
  // 测试 divide_with_ceil 函数的各种情况
  
  // 正常除法
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(15, 5), 3)
  assert_eq(divide_with_ceil(7, 2), 4)
  
  // 边界情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 1), 5)
  assert_eq(divide_with_ceil(10, 10), 1)
  
  // 除数为零的情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 负数测试
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  // 测试 greet 函数的各种输入情况
  
  // 标准输入
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  
  // 特殊输入
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
  
  // 单字符输入
  assert_eq_string(greet("A"), "Hello, A!")
  assert_eq_string(greet(" "), "Hello,  !")
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  // 验证加法和乘法的交换律
  
  let a = 17
  let b = 23
  let c = 0
  let d = -5
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(c, d), add(d, c))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(c, d), multiply(d, c))
  
  // 零元素性质
  assert_eq(add(a, 0), a)
  assert_eq(multiply(a, 0), 0)
}

test "calculation_pipeline_validation" {
  // 计算流水线验证测试
  // 测试多步骤计算的准确性
  
  let base = 100
  let multiplier = 3
  let increment = 25
  let divisor = 5
  
  // 计算流水线
  let step1 = multiply(base, multiplier)      // 100 * 3 = 300
  let step2 = add(step1, increment)           // 300 + 25 = 325
  let step3 = subtract(step2, base)           // 325 - 100 = 225
  let step4 = divide_with_ceil(step3, divisor) // ceil(225/5) = 45
  
  // 验证每一步
  assert_eq(step1, 300)
  assert_eq(step2, 325)
  assert_eq(step3, 225)
  assert_eq(step4, 45)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 系统性测试各种边界条件
  
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 0), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 1), 1)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(1, 1), 1)
  
  // 负单位值测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 1), -1)
  assert_eq(subtract(0, -1), 1)
  assert_eq(divide_with_ceil(-1, 1), -1)
  
  // 大数边界测试
  let large_num = 10000
  assert_eq(add(large_num, large_num), 20000)
  assert_eq(divide_with_ceil(large_num, 3), 3334)
}

test "error_handling_behavior" {
  // 错误处理行为测试
  // 验证函数在异常情况下的行为
  
  // 除零错误处理
  let result1 = divide_with_ceil(50, 0)
  let result2 = divide_with_ceil(-50, 0)
  let result3 = divide_with_ceil(0, 0)
  
  // 验证所有除零情况都返回0
  assert_eq(result1, 0)
  assert_eq(result2, 0)
  assert_eq(result3, 0)
  
  // 极端值测试
  let extreme_positive = 1000000
  let extreme_negative = -1000000
  
  assert_eq(add(extreme_positive, extreme_negative), 0)
  assert_eq(multiply(extreme_positive, 0), 0)
  assert_eq(divide_with_ceil(extreme_positive, extreme_negative), -1)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  // 模拟完整的购物车计算场景
  
  // 商品价格和数量
  let apple_price = 5
  let apple_quantity = 8
  let banana_price = 3
  let banana_quantity = 12
  let orange_price = 7
  let orange_quantity = 6
  
  // 计算各商品小计
  let apple_total = multiply(apple_price, apple_quantity)     // 5 * 8 = 40
  let banana_total = multiply(banana_price, banana_quantity) // 3 * 12 = 36
  let orange_total = multiply(orange_price, orange_quantity) // 7 * 6 = 42
  
  // 计算商品总计
  let subtotal = add(add(apple_total, banana_total), orange_total) // 40 + 36 + 42 = 118
  
  // 计算税费（8%，向上取整）
  let tax_rate = 8
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100) // ceil(118 * 8 / 100) = 10
  
  // 计算总价
  let total = add(subtotal, tax_amount) // 118 + 10 = 128
  
  // 计算每箱包装数量（每箱最多15个商品）
  let total_items = add(add(apple_quantity, banana_quantity), orange_quantity) // 8 + 12 + 6 = 26
  let boxes_needed = divide_with_ceil(total_items, 15) // ceil(26/15) = 2
  
  // 验证计算结果
  assert_eq(apple_total, 40)
  assert_eq(banana_total, 36)
  assert_eq(orange_total, 42)
  assert_eq(subtotal, 118)
  assert_eq(tax_amount, 10)
  assert_eq(total, 128)
  assert_eq(total_items, 26)
  assert_eq(boxes_needed, 2)
}
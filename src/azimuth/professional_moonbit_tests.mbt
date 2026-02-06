// 专业级 MoonBit 测试用例
// 覆盖核心功能和边界情况的标准测试

test "arithmetic_commutativity" {
  // 测试算术运算的交换律
  let a = 42
  let b = 17
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(add(a, b), 59)
  assert_eq(multiply(a, b), 714)
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 15
  
  // 加法单位元: x + 0 = x
  assert_eq(add(x, 0), x)
  assert_eq(add(0, x), x)
  
  // 乘法单位元: x * 1 = x
  assert_eq(multiply(x, 1), x)
  assert_eq(multiply(1, x), x)
  
  // 乘法零元: x * 0 = 0
  assert_eq(multiply(x, 0), 0)
  assert_eq(multiply(0, x), 0)
  
  // 减法恒等式: x - x = 0
  assert_eq(subtract(x, x), 0)
}

test "division_rounding_behavior" {
  // 测试除法向上取整的各种情况
  // 正数除法
  assert_eq(divide_with_ceil(10, 3), 4)    // 3.33... → 4
  assert_eq(divide_with_ceil(10, 4), 3)    // 2.5 → 3
  assert_eq(divide_with_ceil(12, 4), 3)    // 3 → 3
  
  // 负数除法
  assert_eq(divide_with_ceil(-10, 3), -3)  // -3.33... → -3
  assert_eq(divide_with_ceil(10, -3), 3)   // -3.33... → 3
  assert_eq(divide_with_ceil(-10, -3), 4)  // 3.33... → 4
  
  // 零值情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)     // 边界情况
}

test "complex_expression_evaluation" {
  // 测试复杂表达式的求值
  let a = 5
  let b = 3
  let c = 2
  
  // 复合运算: (a + b) * (c + 1)
  let expr1 = multiply(add(a, b), add(c, 1))
  assert_eq(expr1, 16)
  
  // 嵌套运算: a * b + c * a
  let expr2 = add(multiply(a, b), multiply(c, a))
  assert_eq(expr2, 25)
  
  // 除法组合: (a * b + c) / a
  let expr3 = divide_with_ceil(add(multiply(a, b), c), a)
  assert_eq(expr3, 4)
  
  // 减法链: a - b - c
  let expr4 = subtract(subtract(a, b), c)
  assert_eq(expr4, 0)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  // 标准情况
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  
  // 边界情况
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet(" "), "Hello,  !")
  
  // 特殊字符
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
  
  // 长字符串
  let long_name = "VeryLongNameForTesting"
  assert_eq_string(greet(long_name), "Hello, " + long_name + "!")
}

test "numerical_range_validation" {
  // 测试数值范围验证
  // 小范围数值
  assert_eq(add(-5, 5), 0)
  assert_eq(multiply(-3, 3), -9)
  assert_eq(subtract(100, 99), 1)
  
  // 中等范围数值
  assert_eq(add(500, 500), 1000)
  assert_eq(multiply(25, 40), 1000)
  assert_eq(subtract(1000, 1), 999)
  
  // 大数值运算
  assert_eq(add(100000, 200000), 300000)
  assert_eq(multiply(1000, 1000), 1000000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 极值测试
  assert_eq(divide_with_ceil(1, 1000000), 1)
  assert_eq(divide_with_ceil(1000000, 1), 1000000)
  
  // 负数极值
  assert_eq(add(-1000000, 1000000), 0)
  assert_eq(subtract(-1000000, -1000000), 0)
  
  // 混合符号运算
  assert_eq(add(-1000, 500), -500)
  assert_eq(multiply(-100, 10), -1000)
  assert_eq(divide_with_ceil(-100, 3), -33)
}

test "algorithmic_pattern_recognition" {
  // 测试算法模式识别
  // 等差数列求和: 1 + 2 + ... + 10
  let arithmetic_sum = add(add(add(add(add(add(add(add(add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  assert_eq(arithmetic_sum, 55)
  
  // 等比数列: 2, 4, 8, 16
  let geo1 = multiply(2, 2)
  let geo2 = multiply(geo1, 2)
  let geo3 = multiply(geo2, 2)
  assert_eq(geo1, 4)
  assert_eq(geo2, 8)
  assert_eq(geo3, 16)
  
  // 阶乘模拟: 5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(factorial_5, 120)
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算: 商品价格 × 数量 + 运费
  let item_price = 25
  let quantity = 4
  let shipping = 10
  let total_cost = add(multiply(item_price, quantity), shipping)
  assert_eq(total_cost, 110)
  
  // 时间计算: 小时 × 60 + 分钟
  let hours = 2
  let minutes = 45
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(total_minutes, 165)
  
  // 资源分配: 总资源 ÷ 每人需求
  let total_resources = 47
  let per_person = 8
  let people_served = divide_with_ceil(total_resources, per_person)
  assert_eq(people_served, 6)
}
// 用户定制的标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let a = 8
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(22, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(26, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(3, result3)
}

test "string_greeting_variations" {
  // 字符串问候变化测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_comprehensive_edge_cases" {
  // 除法综合边界情况测试
  // 正数除法
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(1, divide_with_ceil(3, 5))
  assert_eq(7, divide_with_ceil(13, 2))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 零值情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_identities_validation" {
  // 数学恒等式验证测试
  let x = 15
  let y = 7
  
  // 加法恒等式
  assert_eq(add(x, 0), x)
  assert_eq(add(0, y), y)
  
  // 乘法恒等式
  assert_eq(multiply(x, 1), x)
  assert_eq(multiply(1, y), y)
  
  // 减法恒等式
  assert_eq(subtract(x, 0), x)
  assert_eq(subtract(x, x), 0)
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "calculation_pipeline_sequence" {
  // 计算流水线序列测试
  let initial = 20
  
  // 步骤1：加倍
  let step1 = multiply(initial, 2)
  assert_eq(40, step1)
  
  // 步骤2：增加15
  let step2 = add(step1, 15)
  assert_eq(55, step2)
  
  // 步骤3：减去5
  let step3 = subtract(step2, 5)
  assert_eq(50, step3)
  
  // 步骤4：除以3向上取整
  let step4 = divide_with_ceil(step3, 3)
  assert_eq(17, step4)
}

test "number_range_operations" {
  // 数值范围操作测试
  let min_val = 1
  let max_val = 100
  
  // 计算范围中点
  let midpoint = divide_with_ceil(add(min_val, max_val), 2)
  assert_eq(51, midpoint)
  
  // 计算范围总和（1+100）* 50
  let range_sum = multiply(divide_with_ceil(add(min_val, max_val), 2), 50)
  assert_eq(2550, range_sum)
  
  // 计算能被7整除的数的数量
  let divisible_by_7 = divide_with_ceil(max_val, 7)
  assert_eq(15, divisible_by_7)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 大数值运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 极值除法
  assert_eq(50000, divide_with_ceil(large_num, 2))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(0, divide_with_ceil(large_num, 200000))
  
  // 负值极值
  let neg_large = -100000
  assert_eq(-200000, add(neg_large, neg_large))
  assert_eq(10000000000, multiply(neg_large, neg_large))
}

test "practical_business_calculations" {
  // 实用商业计算测试
  let unit_price = 25
  let quantity = 8
  let tax_rate = 10
  let discount = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(200, subtotal)
  
  // 计算折扣
  let discount_amount = divide_with_ceil(multiply(subtotal, discount), 100)
  assert_eq(20, discount_amount)
  
  // 计算折后价格
  let discounted_price = subtract(subtotal, discount_amount)
  assert_eq(180, discounted_price)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_rate), 100)
  assert_eq(18, tax_amount)
  
  // 计算总价
  let total_price = add(discounted_price, tax_amount)
  assert_eq(198, total_price)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 50
  
  // O(1) 常数时间操作
  let constant_result = add(10, 5)
  assert_eq(15, constant_result)
  
  // O(n) 线性时间操作模拟：求和1到n
  let linear_sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(1275, linear_sum)
  
  // O(n²) 二次时间操作模拟
  let quadratic_ops = multiply(n, n)
  assert_eq(2500, quadratic_ops)
  
  // O(log n) 对数时间操作模拟
  let log_ops = divide_with_ceil(n, 2)
  assert_eq(25, log_ops)
}

test "comprehensive_function_integration" {
  // 综合函数集成测试
  let base_value = 12
  let multiplier = 3
  let increment = 7
  let divisor = 5
  let name = "User"
  
  // 复杂数值计算
  let step1 = multiply(base_value, multiplier)
  let step2 = add(step1, increment)
  let step3 = subtract(step2, base_value)
  let final_result = divide_with_ceil(step3, divisor)
  
  assert_eq(36, step1)
  assert_eq(43, step2)
  assert_eq(31, step3)
  assert_eq(7, final_result)
  
  // 字符串处理集成
  let greeting = greet(name)
  assert_eq_string("Hello, User!", greeting)
  
  // 数值与字符串处理结合
  let combined_length = add(greeting.length(), name.length())
  assert_eq(17, combined_length)
}
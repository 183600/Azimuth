// 新的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_standard_cases" {
  // 测试加法函数的标准情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "multiply_function_standard_cases" {
  // 测试乘法函数的标准情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "divide_with_ceil_standard_cases" {
  // 测试向上取整除法的标准情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, User!", @azimuth.greet("User"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 8
  let y = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
}

test "edge_case_handling" {
  // 测试边界情况处理
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "real_world_business_calculation" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let a = 10
  let b = 15
  let c = 20
  
  // 测试 (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
  
  // 测试复杂嵌套表达式
  let result3 = @azimuth.add(@azimuth.multiply(@azimuth.divide_with_ceil(a, 3), b), @azimuth.divide_with_ceil(@azimuth.multiply(b, c), a))
  @azimuth.assert_eq(37, result3)  // ceil(10/3) * 15 + ceil(15*20/10) = 4 * 15 + 30 = 60 + 30 = 90
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, John Doe!", greeting)
  
  // 测试特殊字符处理
  let special_name = "Alice & Bob"
  let special_greeting = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, Alice & Bob!", special_greeting)
}

test "comprehensive_functionality_test" {
  // 综合功能测试
  // 库存管理场景
  let items_per_box = 24
  let total_items = 100
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 计算剩余空间
  let remaining_space = @azimuth.multiply(boxes_needed, items_per_box) - total_items
  
  // 计算总成本（假设每个箱子成本为5元）
  let cost_per_box = 5
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)
  @azimuth.assert_eq(20, remaining_space)
  @azimuth.assert_eq(25, total_cost)
  
  // 测试问候功能与业务逻辑结合
  let customer_name = "张三"
  let order_greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", order_greeting)
}
// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本数学运算和字符串处理功能的测试

test "add_function_basic" {
  // 测试基本的加法功能
  let result1 = add(3, 7)
  let result2 = add(0, 0)
  let result3 = add(-2, 5)
  
  // 验证结果
  assert_eq(result1, 10)
  assert_eq(result2, 0)
  assert_eq(result3, 3)
}

test "multiply_function_comprehensive" {
  // 测试乘法功能的多种情况
  let result1 = multiply(4, 5)
  let result2 = multiply(0, 10)
  let result3 = multiply(-3, 6)
  let result4 = multiply(-2, -4)
  
  // 验证结果
  assert_eq(result1, 20)
  assert_eq(result2, 0)
  assert_eq(result3, -18)
  assert_eq(result4, 8)
}

test "greet_function_variations" {
  // 测试问候函数的不同输入
  let greeting1 = greet("MoonBit")
  let greeting2 = greet("")
  let greeting3 = greet("世界")
  
  // 验证结果
  assert_eq_string(greeting1, "Hello, MoonBit!")
  assert_eq_string(greeting2, "Hello, !")
  assert_eq_string(greeting3, "Hello, 世界!")
}

test "divide_with_ceil_positive_cases" {
  // 测试正数的向上取整除法
  let result1 = divide_with_ceil(10, 3)
  let result2 = divide_with_ceil(9, 3)
  let result3 = divide_with_ceil(1, 100)
  let result4 = divide_with_ceil(25, 6)
  
  // 验证结果
  assert_eq(result1, 4)
  assert_eq(result2, 3)
  assert_eq(result3, 1)
  assert_eq(result4, 5)
}

test "divide_with_ceil_negative_and_zero" {
  // 测试负数和零的向上取整除法
  let result1 = divide_with_ceil(-10, 3)
  let result2 = divide_with_ceil(-10, -3)
  let result3 = divide_with_ceil(5, 0)
  let result4 = divide_with_ceil(0, 5)
  
  // 验证结果
  assert_eq(result1, -3)
  assert_eq(result2, 4)
  assert_eq(result3, 0)
  assert_eq(result4, 0)
}

test "combined_operations_business_scenario" {
  // 测试组合运算的业务场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  // 验证结果
  assert_eq(subtotal, 200)
  assert_eq(total, 215)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 验证结果：ceil(47/12) = 4
  assert_eq(boxes_needed, 4)
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  let x = 10
  let y = 20
  let z = 5
  
  // 计算：(x + y) * z - x / y
  let sum_xy = add(x, y)
  let product = multiply(sum_xy, z)
  let division = divide_with_ceil(x, y)
  let result = add(product, -division)
  
  // 验证结果：(10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
  assert_eq(result, 149)
}

test "string_and_number_integration" {
  // 测试字符串和数字的集成处理
  let name = "User"
  let age = 25
  let score = 87
  
  // 生成个性化问候
  let greeting = greet(name)
  
  // 计算总分（年龄 + 分数）
  let total_points = add(age, score)
  
  // 计算平均分（向上取整）
  let average = divide_with_ceil(total_points, 2)
  
  // 验证结果
  assert_eq_string(greeting, "Hello, User!")
  assert_eq(total_points, 112)
  assert_eq(average, 56)
}
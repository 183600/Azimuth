// 标准 MoonBit 测试用例
// 包含对 azimuth 库核心功能的全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试各种输入的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "divide_with_ceil_various_scenarios" {
  // 测试各种场景的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = add(divide_with_ceil(15, 4), multiply(3, 2))
  assert_eq(10, result3)  // ceil(15/4) + 3*2 = 4 + 6 = 10
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极端值
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(100, divide_with_ceil(9999, 100))
}

test "practical_business_calculations" {
  // 实际业务计算测试
  // 计算订单总价
  let item_price = 25
  let quantity = 4
  let shipping = 10
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(110, total)  // 25*4 + 10 = 100 + 10 = 110
  
  // 计算需要的包装箱数量
  let total_items = 35
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(35/8) = 5
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
  
  // 分配律测试
  let a = 3
  let b = 4
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试包含空格的名字
  let name_with_spaces = "John Doe"
  let greeting_spaces = greet(name_with_spaces)
  assert_eq_string("Hello, John Doe!", greeting_spaces)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1：计算项目成本
  let labor_cost = multiply(8, 25)  // 8小时 * 25元/小时
  let material_cost = 150
  let overhead = divide_with_ceil(labor_cost, 4)  // 管理费用为人工成本的25%，向上取整
  let total_cost = add(add(labor_cost, material_cost), overhead)
  assert_eq(370, total_cost)  // 200 + 150 + ceil(200/4) = 200 + 150 + 50 = 400
  
  // 场景2：计算时间安排
  let tasks_per_day = 6
  let total_tasks = 25
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)  // 每天8小时
  assert_eq(40, total_hours)  // ceil(25/6) = 5天, 5*8 = 40小时
}
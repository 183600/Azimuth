// 标准 MoonBit 测试用例 - 针对 azimuth 库的10个高质量测试
// 使用标准的 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-7, 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_operations" {
  // 测试乘法函数的基本运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 8))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 6
  let y = 7
  let z = 8
  
  // 测试 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(104, result1)
  
  // 测试 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(50, result2)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-18, @azimuth.multiply(6, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "real_world_business_calculation" {
  // 测试实际业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(270, subtotal)
  @azimuth.assert_eq(27, tax)
  @azimuth.assert_eq(312, total)
}

test "boundary_value_operations" {
  // 测试边界值运算
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
}

test "complex_practical_scenario" {
  // 测试复杂实际场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 5
  let additional_tasks = 12
  let work_days = 6
  
  // 计算总任务数
  let total_tasks = @azimuth.add(@azimuth.multiply(team_members, tasks_per_member), additional_tasks)
  
  // 计算每天需要完成的任务数
  let tasks_per_day = @azimuth.divide_with_ceil(total_tasks, work_days)
  
  // 计算剩余任务（如果每天完成固定数量）
  let completed_tasks = @azimuth.multiply(tasks_per_day, work_days)
  let remaining_tasks = @azimuth.add(total_tasks, -completed_tasks)
  
  @azimuth.assert_eq(52, total_tasks)
  @azimuth.assert_eq(9, tasks_per_day)
  @azimuth.assert_eq(2, remaining_tasks)
}
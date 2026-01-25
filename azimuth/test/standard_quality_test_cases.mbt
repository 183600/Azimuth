// 标准 MoonBit 测试用例 - 为 azimuth 库编写的高质量测试
// 包含基本功能、边界条件和实际应用场景的测试

// 测试加法函数的基本功能和交换律
test "add_basic_functionality" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(10, @azimuth.add(3, 7))  // 交换律测试
}

// 测试乘法函数的基本功能和交换律
test "multiply_basic_functionality" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(5, 3))  // 交换律测试
}

// 测试问候函数的基本功能
test "greet_basic_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串测试
}

// 测试向上取整除法的基本功能
test "divide_with_ceil_basic_functionality" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除零测试
}

// 测试数学分配律性质
test "mathematical_distributive_property" {
  let a = 6
  let b = 4
  let c = 3
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

// 测试负数运算
test "negative_number_operations" {
  // 加法负数测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  
  // 乘法负数测试
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
  
  // 向上取整除法负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

// 测试Unicode和国际化字符支持
test "unicode_and_internationalization" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

// 测试实际业务场景：包装计算
test "business_packaging_scenario" {
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.add(total_capacity, -total_items)
  @azimuth.assert_eq(1, remaining_space)
}

// 测试复杂计算流水线
test "complex_calculation_pipeline" {
  // 场景：项目资源需求计算
  let team_size = 8
  let tasks_per_person = 5
  let sprint_capacity = 15
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(40, total_tasks)
  @azimuth.assert_eq(3, sprints_needed)
}

// 测试边界条件和极值
test "boundary_conditions_and_extremes" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_val, min_val))
}
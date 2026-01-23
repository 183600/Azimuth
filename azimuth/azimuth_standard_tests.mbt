// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

// 测试加法函数的交换律性质
test "add_commutative_property" {
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-4, @azimuth.add(-7, 3))
  @azimuth.assert_eq(-4, @azimuth.add(3, -7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

// 测试乘法函数的分配律性质
test "multiply_distributive_property" {
  let a = 6
  let b = 4
  let c = 3
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试另一个例子
  let x = 10
  let y = 5
  let z = 2
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

// 测试向上取整除法的实际应用场景
test "divide_with_ceil_packaging_scenario" {
  // 场景：计算装盒问题
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 验证需要的盒子数量
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量足够
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(48, total_capacity)
}

// 测试负数除法的特殊情况
test "divide_with_ceil_negative_cases" {
  // 测试负被除数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 3))
  
  // 测试负除数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, -3))
  
  // 测试双负数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -3))
}

// 测试字符串处理的边界情况
test "greet_function_edge_cases" {
  // 测试空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试复杂计算链
test "complex_calculation_chain" {
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let sprint_capacity = 15
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证结果
  @azimuth.assert_eq(40, total_tasks)
  @azimuth.assert_eq(3, sprints_needed)
}

// 测试数学恒等式
test "mathematical_identities" {
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
  
  // 负数恒等式
  @azimuth.assert_eq(@azimuth.multiply(x, -1), @azimuth.multiply(-1, x))
}

// 测试大数运算
test "large_number_operations" {
  let large_num = 1000000
  
  // 测试大数加法
  @azimuth.assert_eq(2000000, @azimuth.add(large_num, large_num))
  
  // 测试大数乘法
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(large_num, large_num))
  
  // 测试大数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num, large_num))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(large_num + 1, large_num))
}

// 测试Unicode和国际化字符
test "greet_function_unicode" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

// 测试连续计算和业务逻辑
test "business_logic_calculation" {
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  let tables_per_room = 5
  
  // 计算需要的桌子数
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  
  // 计算需要的房间数
  let rooms_needed = @azimuth.divide_with_ceil(tables_needed, tables_per_room)
  
  // 验证结果
  @azimuth.assert_eq(11, tables_needed)
  @azimuth.assert_eq(3, rooms_needed)
  
  // 验证总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
}
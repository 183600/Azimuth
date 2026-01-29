// 标准 MoonBit 核心测试用例
// 为 azimuth 库添加的核心功能测试

// 测试基本算术运算
test "basic_arithmetic_operations" {
  // 测试加法
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  
  // 测试乘法
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  
  // 测试减法
  @azimuth.assert_eq(4, @azimuth.subtract(7, 3))
  @azimuth.assert_eq(-4, @azimuth.subtract(3, 7))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

// 测试除法向上取整功能
test "divide_with_ceil_functionality" {
  // 基本除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

// 测试字符串处理功能
test "string_processing_functions" {
  // 基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

// 测试数学交换律性质
test "mathematical_commutative_properties" {
  let a = 15
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

// 测试业务逻辑场景
test "business_logic_packaging" {
  // 场景：计算装盒问题
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  // 验证需要的盒子数量
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(1, remaining_space)
}

// 测试复杂计算流水线
test "complex_calculation_pipeline" {
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  // 计算每日处理能力
  let daily_capacity = @azimuth.multiply(people, tasks_per_person_per_day)
  
  // 计算需要的天数
  let days_needed = @azimuth.divide_with_ceil(tasks, daily_capacity)
  @azimuth.assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(days_needed, daily_capacity)
  @azimuth.assert_eq(32, total_capacity)
}

// 测试数学恒等式
test "mathematical_identity_properties" {
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

// 测试资源分配优化
test "resource_allocation_optimization" {
  // 场景：服务器资源分配
  let total_memory = 16384  // GB
  let servers = 5
  
  // 计算每台服务器的内存分配
  let memory_per_server = @azimuth.divide_with_ceil(total_memory, servers)
  @azimuth.assert_eq(3277, memory_per_server)  // ceil(16384/5)
  
  // 计算实际分配的总内存
  let total_allocated = @azimuth.multiply(memory_per_server, servers)
  @azimuth.assert_eq(16385, total_allocated)
  
  // 计算内存浪费
  let memory_waste = @azimuth.subtract(total_allocated, total_memory)
  @azimuth.assert_eq(1, memory_waste)
}

// 测试错误处理和边界条件
test "error_handling_and_boundary_conditions" {
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

// 测试真实世界应用场景
test "real_world_event_planning" {
  // 场景：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  let tables_per_room = 5
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算需要的房间数量
  let rooms_needed = @azimuth.divide_with_ceil(tables_needed, tables_per_room)
  @azimuth.assert_eq(3, rooms_needed)  // ceil(11/5) = 3
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 验证座位充足
  @azimuth.assert_true(total_seats >= participants)
}
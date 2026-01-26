// 标准 MoonBit 测试用例 - 高质量测试集
// 为 azimuth 库添加额外的测试覆盖

test "add_function_extended" {
  // 扩展的加法函数测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(4, @azimuth.add(-7, 11))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_extended" {
  // 扩展的乘法函数测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  @azimuth.assert_eq(21, @azimuth.multiply(-7, -3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "divide_with_ceil_extended" {
  // 扩展的向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_extended" {
  // 扩展的问候函数测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "subtract_function_basic" {
  // 基本的减法函数测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
  @azimuth.assert_eq(-15, @azimuth.subtract(-10, 5))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let a = @azimuth.add(5, 3)
  let b = @azimuth.multiply(a, 2)
  let c = @azimuth.divide_with_ceil(b, 4)
  @azimuth.assert_eq(4, c)  // ((5+3)*2)/4 = 16/4 = 4
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 10
  let y = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 2)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 2))
  )
}

test "business_logic_scenario" {
  // 业务逻辑场景测试
  let items = 25
  let box_capacity = 6
  let boxes_needed = @azimuth.divide_with_ceil(items, box_capacity)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(30, total_capacity)  // 5 * 6 = 30
  
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(5, remaining_space)  // 30 - 25 = 5
}

test "error_handling_test" {
  // 错误处理测试
  // 除零错误
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "sequential_operations" {
  // 连续操作测试
  let initial = 10
  let step1 = @azimuth.add(initial, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)     // 30
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 4
  let step4 = @azimuth.add(step3, 1)          // 5
  
  @azimuth.assert_eq(5, step4)
}
  @azimuth.assert_eq(left_side, right_side)  // 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
}

test "divide_with_ceil_real_world_application" {
  // 真实世界应用：计算装盒问题
  let total_items = 37
  let box_capacity = 8
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(37/8) = 5
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(40, total_capacity)
}

test "greet_function_internationalization" {
  // 测试国际化字符支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "complex_business_calculation" {
  // 复杂业务计算：项目资源规划
  let team_members = 6
  let tasks_per_member = 8
  let sprint_capacity = 15
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  @azimuth.assert_eq(48, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(max_int - 1, @azimuth.add(max_int - 2, 1))
  @azimuth.assert_eq(min_int + 1, @azimuth.add(min_int + 2, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}

test "divide_with_ceil_negative_scenarios" {
  // 向上取整除法的负数场景
  // 负被除数
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
  
  // 负除数
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -4))
  
  // 双负数
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-7, -4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-3, -4))
}

test "sequential_operations_pipeline" {
  // 连续操作流水线测试
  let initial_value = 10
  
  // 操作序列：加法 -> 乘法 -> 除法 -> 加法
  let step1 = @azimuth.add(initial_value, 5)        // 15
  let step2 = @azimuth.multiply(step1, 3)           // 45
  let step3 = @azimuth.divide_with_ceil(step2, 8)   // 6
  let step4 = @azimuth.add(step3, 2)                // 8
  
  @azimuth.assert_eq(8, step4)
}

test "error_handling_edge_cases" {
  // 错误处理边界情况
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2147483647, 2147483647))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-2147483648, -2147483648))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  
  // 数字和字母组合
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 特殊符号
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, &*()!", @azimuth.greet("&*()"))
}
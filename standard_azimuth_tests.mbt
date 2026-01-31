// 标准 MoonBit 测试用例 - Azimuth 库
// 测试核心功能函数

test "add_basic_functionality" {
  // 测试基本加法功能
  @assertion.assert_eq(5, add(2, 3))?
  @assertion.assert_eq(0, add(0, 0))?
  @assertion.assert_eq(-5, add(-2, -3))?
  @assertion.assert_eq(10, add(-5, 15))?
}

test "multiply_comprehensive_test" {
  // 测试乘法功能的各种情况
  @assertion.assert_eq(6, multiply(2, 3))?
  @assertion.assert_eq(0, multiply(5, 0))?
  @assertion.assert_eq(-15, multiply(-5, 3))?
  @assertion.assert_eq(15, multiply(-5, -3))?
  @assertion.assert_eq(100, multiply(10, 10))?
}

test "greet_function_variations" {
  // 测试问候函数的不同输入
  @assertion.assert_eq("Hello, World!", greet("World"))?
  @assertion.assert_eq("Hello, MoonBit!", greet("MoonBit"))?
  @assertion.assert_eq("Hello, !", greet(""))?
  @assertion.assert_eq("Hello, 123!", greet("123"))?
  @assertion.assert_eq("Hello, 世界!", greet("世界"))?
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑
  // 包装场景：计算需要的箱子数量
  @assertion.assert_eq(4, divide_with_ceil(25, 8))?  // ceil(25/8) = 4
  @assertion.assert_eq(3, divide_with_ceil(24, 8))?  // ceil(24/8) = 3
  @assertion.assert_eq(1, divide_with_ceil(1, 10))?  // ceil(1/10) = 1
  @assertion.assert_eq(0, divide_with_ceil(10, 0))?  // 除零保护
  @assertion.assert_eq(5, divide_with_ceil(-10, -3))?  // ceil(-10/-3) = 4
}

test "subtract_function_test" {
  // 测试减法功能
  @assertion.assert_eq(5, subtract(10, 5))?
  @assertion.assert_eq(-5, subtract(5, 10))?
  @assertion.assert_eq(0, subtract(5, 5))?
  @assertion.assert_eq(15, subtract(-5, -20))?
  @assertion.assert_eq(-15, subtract(5, 20))?
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：项目资源计算
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  @assertion.assert_eq(40, total_tasks)?      // 8 * 5 = 40
  @assertion.assert_eq(3, sprints_needed)?    // ceil(40/15) = 3
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  @assertion.assert_eq(add(x, y), add(y, x))?
  
  // 乘法交换律
  @assertion.assert_eq(multiply(x, y), multiply(y, x))?
  
  // 分配律
  @assertion.assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )?
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  @assertion.assert_eq(11, tables_needed)?  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  @assertion.assert_eq(88, total_seats)?
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  @assertion.assert_eq(1, remaining_seats)?
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  @assertion.assert_eq(0, divide_with_ceil(10, 0))?
  @assertion.assert_eq(0, divide_with_ceil(-10, 0))?
  @assertion.assert_eq(0, divide_with_ceil(0, 0))?
  
  // 极值测试
  @assertion.assert_eq(1, divide_with_ceil(1, 1))?
  @assertion.assert_eq(1, divide_with_ceil(-1, -1))?
  @assertion.assert_eq(0, divide_with_ceil(0, 1))?
}
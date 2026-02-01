// 标准 MoonBit 测试用例
// 包含10个高质量的测试用例，使用标准 MoonBit 测试语法

// 测试基本的整数运算
test basic_integer_operations {
  // 测试加法函数
  assert_eq(5, add(2, 3))
  assert_eq(25, add(10, 15))
  assert_eq(-2, add(-5, 3))
  
  // 测试减法函数
  assert_eq(7, subtract(10, 3))
  assert_eq(-3, subtract(5, 8))
  assert_eq(2, subtract(-3, -5))
  
  // 测试乘法函数
  assert_eq(20, multiply(4, 5))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(0, multiply(0, 100))
}

// 测试字符串操作
test string_operations {
  // 测试字符串拼接函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

// 测试数学运算
test mathematical_operations {
  // 测试向上取整除法函数
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 测试负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

// 测试边界情况
test boundary_cases {
  // 测试零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  
  // 测试极值
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(0, divide_with_ceil(0, max_int))
}

// 测试数学恒等式
test mathematical_identities {
  // 测试加法交换律
  let a = 15
  let b = 8
  assert_eq(add(a, b), add(b, a))
  
  // 测试乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试分配律
  assert_eq(
    multiply(a, add(b, 3)),
    add(multiply(a, b), multiply(a, 3))
  )
}

// 测试业务逻辑场景
test business_logic_scenarios {
  // 场景：包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

// 测试复杂计算流水线
test complex_calculation_pipeline {
  // 场景：项目资源计算
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)  // 8 * 5 = 40
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

// 测试错误处理和特殊情况
test error_handling_and_special_cases {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  let large_number = 1000000
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

// 测试连续操作组合
test sequential_operations {
  // 测试连续的数学运算
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)          // 5
  
  assert_eq(15, step1)
  assert_eq(30, step2)
  assert_eq(4, step3)
  assert_eq(5, step4)
}

// 测试真实世界应用场景
test real_world_application_scenarios {
  // 场景：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证座位充足性
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
}
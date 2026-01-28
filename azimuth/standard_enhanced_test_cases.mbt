// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_identity_properties" {
  // 测试加法函数的恒等性质
  assert_eq(5, add(5, 0))  // 加法单位元
  assert_eq(5, add(0, 5))  // 交换律验证
  assert_eq(-5, add(-5, 0))  // 负数单位元
}

test "multiply_function_distributive_law" {
  // 测试乘法函数的分配律
  let a = 6
  let b = 4
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "divide_with_ceil_floor_relationship" {
  // 测试向上取整除法与普通除法的关系
  assert_eq(2, divide_with_ceil(4, 2))  // 整除情况
  assert_eq(3, divide_with_ceil(5, 2))  // 向上取整
  assert_eq(1, divide_with_ceil(1, 2))  // 小于1的情况
  assert_eq(0, divide_with_ceil(0, 5))  // 零被除数
}

test "subtract_function_complementary" {
  // 测试减法函数的互补性质
  let x = 15
  let y = 8
  
  // x - y + y = x
  let result = add(subtract(x, y), y)
  assert_eq(x, result)
  
  // x - x = 0
  assert_eq(0, subtract(x, x))
}

test "greet_function_empty_and_special_inputs" {
  // 测试问候函数的特殊输入处理
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("   "))  // 空白字符
  assert_eq_string("Hello, newline\n!", greet("newline\n"))  // 包含换行
  assert_eq_string("Hello, tab\t!", greet("tab\t"))  // 包含制表符
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目资源和时间需求
  let team_size = 6
  let tasks_per_person = 8
  let work_days = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_day = divide_with_ceil(total_tasks, work_days)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(10, tasks_per_day)  // ceil(48/5) = 10
}

test "mathematical_boundary_values" {
  // 测试数学边界值
  assert_eq(1, divide_with_ceil(1, 1))  // 最小正整数
  assert_eq(-1, divide_with_ceil(-1, 1))  // 最小负整数
  assert_eq(0, divide_with_ceil(0, 1))  // 零值
  assert_eq(1, divide_with_ceil(2, 2))  // 相等数相除
  assert_eq(2, divide_with_ceil(3, 2))  // 最小向上取整情况
}

test "resource_allocation_optimization" {
  // 测试资源分配优化算法
  let total_items = 100
  let container_capacity = 15
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  // 验证容器数量
  assert_eq(7, containers_needed)  // ceil(100/15) = 7
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(105, total_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(5, unused_space)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(10, 0))  // 正数除零
  assert_eq(0, divide_with_ceil(-10, 0))  // 负数除零
  assert_eq(0, divide_with_ceil(0, 0))  // 零除零
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1000000, 1000000))  // 大数相等
  assert_eq(2, divide_with_ceil(1000001, 1000000))  // 大数向上取整
}

test "real_world_business_scenario" {
  // 真实世界业务场景测试
  // 场景：电商订单打包和配送
  let orders = 47
  let items_per_box = 8
  let boxes_per_truck = 6
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(orders, items_per_box)
  assert_eq(6, boxes_needed)  // ceil(47/8) = 6
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(boxes_needed, boxes_per_truck)
  assert_eq(1, trucks_needed)  // ceil(6/6) = 1
  
  // 计算总容量利用率
  let total_box_capacity = multiply(boxes_needed, items_per_box)
  let utilization = divide_with_ceil(multiply(orders, 100), total_box_capacity)
  assert_eq(98, utilization)  // floor(47*100/48) = 98%
}
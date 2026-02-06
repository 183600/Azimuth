// 单个测试文件 - 包含8个高质量的MoonBit测试用例

test "single_test" {
  assert_eq(5, add(2, 3))
}

test "number_system_conversions" {
  // 模拟数字系统转换计算
  let decimal_value = 255
  let hex_weight = 16
  
  // 计算二进制表示的权重和
  let binary_sum = add(multiply(1, 128), multiply(1, 64))
  let binary_total = add(binary_sum, add(multiply(1, 32), multiply(1, 16)))
  
  // 计算十六进制表示
  let hex_high = divide_with_ceil(decimal_value, hex_weight)
  let hex_low = decimal_value - multiply(hex_high - 1, hex_weight)
  
  assert_eq(240, binary_total)
  assert_eq(16, hex_high)
  assert_eq(15, hex_low)
}

test "statistical_computation" {
  // 统计分析测试
  let sum_manual = add(add(add(add(add(add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean_value = divide_with_ceil(sum_manual, count)
  
  // 计算中位数近似值
  let median_approx = divide_with_ceil(add(21, 24), 2)
  
  assert_eq(147, sum_manual)
  assert_eq(21, mean_value)
  assert_eq(23, median_approx)
}

test "resource_allocation" {
  // 资源优化算法测试
  let total_resources = 100
  let task1_requirement = 23
  let task2_requirement = 31
  let task3_requirement = 19
  
  // 计算已分配资源
  let allocated = add(add(task1_requirement, task2_requirement), task3_requirement)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, allocated)
  
  // 计算可以支持的新任务数量（每个任务需要8个资源）
  let new_tasks_possible = divide_with_ceil(remaining, 8)
  
  assert_eq(73, allocated)
  assert_eq(27, remaining)
  assert_eq(4, new_tasks_possible)
}

test "geometric_computations" {
  // 几何计算测试
  let length = 8
  let width = 6
  let height = 4
  
  // 计算表面积
  let area1 = multiply(length, width)
  let area2 = multiply(length, height)
  let area3 = multiply(width, height)
  let surface_area = multiply(2, add(add(area1, area2), area3))
  
  // 计算体积
  let volume = multiply(area1, height)
  
  // 计算空间利用率（假设物品体积为150）
  let item_volume = 150
  let utilization = divide_with_ceil(multiply(item_volume, 100), volume)
  
  assert_eq(208, surface_area)
  assert_eq(192, volume)
  assert_eq(78, utilization)
}

test "temporal_analysis" {
  // 时间序列分析测试
  let baseline = 100
  let growth_rate = 15
  
  // 计算复合增长
  let period1 = add(baseline, divide_with_ceil(multiply(baseline, growth_rate), 100))
  let period2 = add(period1, divide_with_ceil(multiply(period1, growth_rate), 100))
  let period3 = add(period2, divide_with_ceil(multiply(period2, growth_rate), 100))
  
  // 计算总增长
  let total_growth = subtract(period3, baseline)
  let growth_percentage = divide_with_ceil(multiply(total_growth, 100), baseline)
  
  assert_eq(115, period1)
  assert_eq(133, period2)
  assert_eq(153, period3)
  assert_eq(53, total_growth)
  assert_eq(53, growth_percentage)
}

test "boundary_validation" {
  // 错误边界条件测试
  let max_safe_int = 1000000
  let min_safe_int = -1000000
  
  // 测试接近边界的运算
  let boundary_add1 = add(max_safe_int, 1)
  let boundary_add2 = add(min_safe_int, -1)
  let boundary_mult = divide_with_ceil(max_safe_int, 1000)
  let boundary_div = divide_with_ceil(max_safe_int, 1)
  
  // 测试零的边界情况
  let zero_by_positive = divide_with_ceil(0, 100)
  let zero_by_negative = divide_with_ceil(0, -100)
  let zero_by_zero = divide_with_ceil(0, 0)
  
  assert_eq(1000001, boundary_add1)
  assert_eq(-1000001, boundary_add2)
  assert_eq(1000, boundary_mult)
  assert_eq(1000000, boundary_div)
  assert_eq(0, zero_by_positive)
  assert_eq(0, zero_by_negative)
  assert_eq(0, zero_by_zero)
}

test "encryption_simulation" {
  // 密码学模拟测试
  let message = 12345
  let key1 = 7
  let key2 = 13
  let modulus = 1000
  
  // 简单的加密过程
  let step1 = multiply(message, key1)
  let step2 = add(step1, key2)
  let encrypted = step2 - multiply(divide_with_ceil(step2, modulus), modulus)
  
  // 解密过程（简化）
  let decrypted_step1 = subtract(encrypted, key2)
  let decrypted_approx = divide_with_ceil(multiply(decrypted_step1, 1), key1)
  
  assert_eq(86425, step1)
  assert_eq(86438, step2)
  assert_eq(438, encrypted)
  assert_eq(425, decrypted_step1)
  assert_eq(61, decrypted_approx)
}

test "physics_modeling" {
  // 物理模拟测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  let mass = 50
  
  // 计算最终速度: v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移: s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  
  // 计算动能: KE = 0.5mv²
  let kinetic_energy = divide_with_ceil(multiply(mass, multiply(final_velocity, final_velocity)), 2)
  
  // 计算需要的燃料单位（每单位燃料提供100能量）
  let fuel_units = divide_with_ceil(kinetic_energy, 100)
  
  assert_eq(20, final_velocity)
  assert_eq(75, displacement)
  assert_eq(10000, kinetic_energy)
  assert_eq(100, fuel_units)
}

test "add_function_basic" {
  // 基本加法测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_commutative" {
  // 乘法交换律测试
  assert_eq(21, multiply(7, 3))
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
}

test "greet_function_standard" {
  // 问候函数标准测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_zero_divisor" {
  // 除零测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "subtract_function_basic" {
  // 基本减法测试
  assert_eq(4, subtract(7, 3))
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(5, 5))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  assert_eq(4, c)  // ((5+3)*2)/4 = 16/4 = 4
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
}

// 新增的10个标准 MoonBit 测试用例
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_properties_extended" {
  // 测试数学性质扩展
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "greeting_functionality_extended" {
  // 测试问候功能扩展
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, User123!", greet("User123"))
}

test "divide_with_ceil_edge_cases_extended" {
  // 测试向上取整除法的边界情况扩展
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "negative_number_operations_extended" {
  // 测试负数运算扩展
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_shopping_cart" {
  // 模拟购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total = add(subtotal1, subtotal2)
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
}

test "packaging_calculation_extended" {
  // 包装计算场景扩展
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "complex_calculations_extended" {
  // 复杂计算测试扩展
  let result = add(
    multiply(5, divide_with_ceil(25, 4)),
    multiply(3, divide_with_ceil(17, 5))
  )
  assert_eq(43, result)  // 5*7 + 3*4 = 35 + 12 = 47
}

test "large_number_operations_extended" {
  // 大数运算测试扩展
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "business_order_calculation_extended" {
  // 业务订单计算扩展
  let unit_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

// 用户自定义的标准 MoonBit 测试用例
test "user_custom_test_1" {
  // 基本加法测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "user_custom_test_2" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "user_custom_test_3" {
  // 向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "user_custom_test_4" {
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "user_custom_test_5" {
  // 问候函数测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "user_custom_test_6" {
  // 减法函数测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "user_custom_test_7" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "user_custom_test_8" {
  // 业务场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
}

test "user_custom_test_9" {
  // 复杂工作流测试
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
}

test "user_custom_test_10" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候
  assert_eq_string("Hello,   !", greet("  "))
}
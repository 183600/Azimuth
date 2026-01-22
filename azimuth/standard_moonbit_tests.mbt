// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(-500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(1, 12345))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为0的情况
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算总价：商品价格 * 数量 + 税费
  let price = 25
  let quantity = 4
  let tax = 15
  let total = add(multiply(price, quantity), tax)
  assert_eq(115, total)
  
  // 计算需要的容器数量
  let items = 47
  let container_capacity = 10
  let containers = divide_with_ceil(items, container_capacity)
  assert_eq(5, containers)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  assert_eq(0, divide_with_ceil(0, 0))  // 0除以0
  assert_eq(0, divide_with_ceil(100, 0))  // 正数除以0
  assert_eq(0, divide_with_ceil(-100, 0))  // 负数除以0
  
  // 极值测试
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(1, multiply(1, 1))
}

test "string_operations_comprehensive" {
  // 测试字符串操作的综合情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, special@#$%chars!", greet("special@#$%chars"))
}

test "complex_calculation_workflows" {
  // 测试复杂计算工作流
  // 场景：计算购物车的总容器需求
  let items_per_cart = [15, 8, 12, 7]
  let container_size = 10
  
  let cart1_containers = divide_with_ceil(items_per_cart[0], container_size)
  let cart2_containers = divide_with_ceil(items_per_cart[1], container_size)
  let cart3_containers = divide_with_ceil(items_per_cart[2], container_size)
  let cart4_containers = divide_with_ceil(items_per_cart[3], container_size)
  
  let total_containers = add(add(cart1_containers, cart2_containers), 
                             add(cart3_containers, cart4_containers))
  
  assert_eq(6, total_containers)  // 2+1+2+1 = 6
}

test "performance_and_stress" {
  // 测试性能和压力情况
  // 大数运算
  let big_num1 = 50000
  let big_num2 = 50000
  assert_eq(2500000000, multiply(big_num1, big_num2))
  assert_eq(100000, add(big_num1, big_num2))
  
  // 复杂嵌套计算
  let result = divide_with_ceil(multiply(add(10, 5), 3), 7)
  assert_eq(7, result)  // ceil((10+5)*3/7) = ceil(45/7) = 7
}
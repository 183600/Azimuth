// Azimuth 标准测试套件
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_addition" {
  // 基础算术加法测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(1000, add(500, 500))
}

test "basic_arithmetic_multiplication" {
  // 基础算术乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "ceil_division_basic_functionality" {
  // 向上取整除法基础功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-2, add(3, -5))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "zero_element_properties" {
  // 零元素性质测试
  let x = 42
  
  // 加法零元素
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法零元素
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(x, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1：计算表达式结果
  let result1 = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result1)  // 2*4 + 3*5 = 8 + 15 = 23
  
  // 场景2：嵌套计算
  let result2 = divide_with_ceil(add(15, 10), 8)
  assert_eq(4, result2)  // ceil(25/8) = 4
  
  // 场景3：连续运算
  let step1 = multiply(6, 3)
  let step2 = add(step1, 12)
  let step3 = divide_with_ceil(step2, 5)
  assert_eq(6, step3)  // ceil((6*3+12)/5) = ceil(30/5) = 6
}

test "real_world_packaging_problem" {
  // 真实世界包装问题测试
  // 场景：需要将物品装箱，计算需要的箱子数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, émoji 🎉!", greet("émoji 🎉"))
}
// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 基本加法功能测试
  inspect(add(2, 3), content="5")
  inspect(add(0, 0), content="0")
  inspect(add(-3, 2), content="-1")
  inspect(add(50, 50), content="100")
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  inspect(multiply(2, 3), content="6")
  inspect(multiply(5, 0), content="0")
  inspect(multiply(-2, 3), content="-6")
  inspect(multiply(-3, -5), content="15")
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 5), content="1")
  inspect(divide_with_ceil(5, 0), content="0")
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(10, -3), content="-4")
  inspect(divide_with_ceil(-10, -3), content="4")
  inspect(divide_with_ceil(-1, 2), content="-1")
}

test "greet_function_standard_cases" {
  // 问候函数标准测试
  inspect(greet("World"), content="Hello, World!")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
}

test "subtract_function_basic_cases" {
  // 减法函数基本测试
  inspect(subtract(5, 3), content="2")
  inspect(subtract(3, 5), content="-2")
  inspect(subtract(5, 5), content="0")
  inspect(subtract(0, -10), content="10")
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  inspect(add(a, b), content=to_string(add(b, a)))
  
  // 乘法交换律
  inspect(multiply(a, b), content=to_string(multiply(b, a)))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  inspect(packages_needed, content="5")
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  inspect(remaining_space, content="5")
}
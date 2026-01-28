// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

// 导入被测试的模块
use azimuth

// 测试基本加法运算
test basic_addition {
  azimuth.assert_eq(5, azimuth.add(2, 3))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-1, azimuth.add(2, -3))
}

// 测试基本乘法运算
test basic_multiplication {
  azimuth.assert_eq(6, azimuth.multiply(2, 3))
  azimuth.assert_eq(0, azimuth.multiply(0, 5))
  azimuth.assert_eq(-6, azimuth.multiply(2, -3))
}

// 测试基本减法运算
test basic_subtraction {
  azimuth.assert_eq(2, azimuth.subtract(5, 3))
  azimuth.assert_eq(0, azimuth.subtract(3, 3))
  azimuth.assert_eq(-2, azimuth.subtract(3, 5))
}

// 测试向上取整除法 - 整除情况
test divide_with_ceil_exact_division {
  azimuth.assert_eq(3, azimuth.divide_with_ceil(6, 2))
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-6, 2))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
}

// 测试向上取整除法 - 需要向上取整情况
test divide_with_ceil_rounding_up {
  azimuth.assert_eq(4, azimuth.divide_with_ceil(7, 2))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(8, 3))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
}

// 测试字符串问候功能
test greet_functionality {
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

// 测试数学交换律
test mathematical_commutative_property {
  let a = 7
  let b = 13
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
}

// 测试零元素性质
test zero_element_properties {
  let x = 42
  azimuth.assert_eq(x, azimuth.add(x, 0))
  azimuth.assert_eq(0, azimuth.multiply(x, 0))
}

// 测试复杂计算场景
test complex_calculation {
  let result = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  azimuth.assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

// 测试包装计算场景
test packaging_calculation {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}
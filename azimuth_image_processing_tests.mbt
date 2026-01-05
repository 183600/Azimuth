// Azimuth 图像处理测试用例
// 专注于图像处理基础算法和概念

// 测试1: 基础图像操作
test "基础图像操作" {
  // 简化的图像表示（灰度图像）
  type Image = {
    width: Int,
    height: Int,
    pixels: Array<Array<Int>>  // 像素值 0-255
  }
  
  // 创建空白图像
  let create_blank_image = fn(width: Int, height: Int, color: Int) {
    let mut pixels = []
    
    for y in 0..height {
      let mut row = []
      for x in 0..width {
        row = row.push(color)
      }
      pixels = pixels.push(row)
    }
    
    { width, height, pixels }
  }
  
  // 获取像素值
  let get_pixel = fn(image: Image, x: Int, y: Int) {
    if x >= 0 && x < image.width && y >= 0 && y < image.height {
      Some(image.pixels[y][x])
    } else {
      None
    }
  }
  
  // 设置像素值
  let set_pixel = fn(image: Image, x: Int, y: Int, value: Int) {
    if x >= 0 && x < image.width && y >= 0 && y < image.height {
      let mut new_pixels = image.pixels
      let mut new_row = new_pixels[y]
      new_row[x] = value
      new_pixels[y] = new_row
      
      {
        width: image.width,
        height: image.height,
        pixels: new_pixels
      }
    } else {
      image
    }
  }
  
  // 图像裁剪
  let crop_image = fn(image: Image, x: Int, y: Int, width: Int, height: Int) {
    let mut cropped_pixels = []
    
    for j in 0..height {
      if y + j < image.height {
        let mut row = []
        for i in 0..width {
          if x + i < image.width {
            row = row.push(image.pixels[y + j][x + i])
          } else {
            row = row.push(0)  // 边界外填充黑色
          }
        }
        cropped_pixels = cropped_pixels.push(row)
      }
    }
    
    { width, height, pixels: cropped_pixels }
  }
  
  // 图像缩放（最近邻插值）
  let resize_image = fn(image: Image, new_width: Int, new_height: Int) {
    let mut resized_pixels = []
    
    let x_ratio = image.width.to_float() / new_width.to_float()
    let y_ratio = image.height.to_float() / new_height.to_float()
    
    for j in 0..new_height {
      let mut row = []
      for i in 0..new_width {
        let src_x = (i.to_float() * x_ratio).to_int()
        let src_y = (j.to_float() * y_ratio).to_int()
        
        let pixel_value = if src_x < image.width && src_y < image.height {
          image.pixels[src_y][src_x]
        } else {
          0
        }
        
        row = row.push(pixel_value)
      }
      resized_pixels = resized_pixels.push(row)
    }
    
    { width: new_width, height: new_height, pixels: resized_pixels }
  }
  
  // 图像旋转90度
  let rotate_image_90 = fn(image: Image) {
    let mut rotated_pixels = []
    
    for i in 0..image.width {
      let mut row = []
      for j in 0..image.height {
        row = row.push(image.pixels[image.height - 1 - j][i])
      }
      rotated_pixels = rotated_pixels.push(row)
    }
    
    { width: image.height, height: image.width, pixels: rotated_pixels }
  }
  
  // 测试图像创建
  let blank_image = create_blank_image(5, 5, 128)
  
  assert_eq(blank_image.width, 5)
  assert_eq(blank_image.height, 5)
  assert_eq(blank_image.pixels.length(), 5)
  assert_eq(blank_image.pixels[0].length(), 5)
  assert_eq(blank_image.pixels[2][2], 128)
  
  // 测试像素获取和设置
  let pixel_value = get_pixel(blank_image, 2, 2)
  assert_eq(pixel_value, Some(128))
  
  let out_of_bounds = get_pixel(blank_image, 10, 10)
  assert_eq(out_of_bounds, None)
  
  let modified_image = set_pixel(blank_image, 1, 1, 255)
  let new_pixel_value = get_pixel(modified_image, 1, 1)
  assert_eq(new_pixel_value, Some(255))
  
  // 测试图像裁剪
  let cropped_image = crop_image(modified_image, 1, 1, 3, 3)
  
  assert_eq(cropped_image.width, 3)
  assert_eq(cropped_image.height, 3)
  assert_eq(cropped_image.pixels[0][0], 255)  // 原图像(1,1)位置的像素
  
  // 测试图像缩放
  let resized_image = resize_image(blank_image, 10, 10)
  
  assert_eq(resized_image.width, 10)
  assert_eq(resized_image.height, 10)
  assert_eq(resized_image.pixels.length(), 10)
  assert_eq(resized_image.pixels[0].length(), 10)
  
  // 测试图像旋转
  let rotated_image = rotate_image_90(blank_image)
  
  assert_eq(rotated_image.width, 5)
  assert_eq(rotated_image.height, 5)
  assert_eq(rotated_image.pixels[0][4], 128)  // 原图像(0,0)位置的像素现在在(0,4)
}

// 测试2: 图像滤波器
test "图像滤波器" {
  // 图像表示
  type Image = {
    width: Int,
    height: Int,
    pixels: Array<Array<Int>>
  }
  
  // 卷积核
  type Kernel = {
    size: Int,
    weights: Array<Array<Float>>
  }
  
  // 应用卷积核
  let apply_kernel = fn(image: Image, kernel: Kernel) {
    let mut result_pixels = []
    let offset = kernel.size / 2
    
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let mut sum = 0.0
        
        for ky in 0..kernel.size {
          for kx in 0..kernel.size {
            let px = x + kx - offset
            let py = y + ky - offset
            
            let pixel_value = if px >= 0 && px < image.width && py >= 0 && py < image.height {
              image.pixels[py][px].to_float()
            } else {
              0.0  // 边界外填充0
            }
            
            sum = sum + pixel_value * kernel.weights[ky][kx]
          }
        }
        
        // 限制像素值在0-255范围内
        let clamped_value = if sum < 0.0 {
          0
        } else if sum > 255.0 {
          255
        } else {
          sum.to_int()
        }
        
        row = row.push(clamped_value)
      }
      result_pixels = result_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: result_pixels }
  }
  
  // 创建高斯模糊核
  let create_gaussian_kernel = fn(size: Int, sigma: Float) {
    let mut kernel = []
    let center = size / 2
    let mut sum = 0.0
    
    // 计算高斯值
    for y in 0..size {
      let mut row = []
      for x in 0..size {
        let dx = (x - center).to_float()
        let dy = (y - center).to_float()
        let value = (-(dx * dx + dy * dy) / (2.0 * sigma * sigma)).exp() / (2.0 * 3.14159 * sigma * sigma)
        row = row.push(value)
        sum = sum + value
      }
      kernel = kernel.push(row)
    }
    
    // 归一化
    let mut normalized_kernel = []
    for y in 0..size {
      let mut row = []
      for x in 0..size {
        let normalized_value = kernel[y][x] / sum
        row = row.push(normalized_value)
      }
      normalized_kernel = normalized_kernel.push(row)
    }
    
    { size, weights: normalized_kernel }
  }
  
  // 创建均值滤波核
  let create_mean_kernel = fn(size: Int) {
    let mut kernel = []
    let value = 1.0 / (size * size).to_float()
    
    for y in 0..size {
      let mut row = []
      for x in 0..size {
        row = row.push(value)
      }
      kernel = kernel.push(row)
    }
    
    { size, weights: kernel }
  }
  
  // 创建锐化核
  let create_sharpen_kernel = fn() {
    let kernel = [
      [0.0, -1.0, 0.0],
      [-1.0, 5.0, -1.0],
      [0.0, -1.0, 0.0]
    ]
    
    { size: 3, weights: kernel }
  }
  
  // 创建边缘检测核（Sobel算子）
  let create_sobel_kernel = fn(direction: String) {
    let kernel = if direction == "x" {
      [
        [-1.0, 0.0, 1.0],
        [-2.0, 0.0, 2.0],
        [-1.0, 0.0, 1.0]
      ]
    } else {
      [
        [-1.0, -2.0, -1.0],
        [0.0, 0.0, 0.0],
        [1.0, 2.0, 1.0]
      ]
    }
    
    { size: 3, weights: kernel }
  }
  
  // 创建测试图像
  let create_test_image = fn() {
    let pixels = [
      [10, 10, 10, 10, 10],
      [10, 100, 100, 100, 10],
      [10, 100, 200, 100, 10],
      [10, 100, 100, 100, 10],
      [10, 10, 10, 10, 10]
    ]
    
    { width: 5, height: 5, pixels }
  }
  
  // 测试均值滤波
  let test_image = create_test_image()
  let mean_kernel = create_mean_kernel(3)
  let blurred_image = apply_kernel(test_image, mean_kernel)
  
  // 中心像素(2,2)应该被周围像素的平均值模糊
  // 原值200，周围有8个100和4个10
  // (8*100 + 4*10) / 9 = (800 + 40) / 9 ≈ 93.33
  assert_true(blurred_image.pixels[2][2] > 90 && blurred_image.pixels[2][2] < 95)
  
  // 测试高斯模糊
  let gaussian_kernel = create_gaussian_kernel(3, 1.0)
  let gaussian_blurred = apply_kernel(test_image, gaussian_kernel)
  
  // 高斯模糊也应该使中心像素值降低
  assert_true(gaussian_blurred.pixels[2][2] < 200)
  
  // 测试锐化
  let sharpen_kernel = create_sharpen_kernel()
  let sharpened_image = apply_kernel(test_image, sharpen_kernel)
  
  // 锐化应该增强中心像素
  assert_true(sharpened_image.pixels[2][2] > test_image.pixels[2][2])
  
  // 测试边缘检测
  let sobel_x_kernel = create_sobel_kernel("x")
  let sobel_y_kernel = create_sobel_kernel("y")
  
  let edge_x = apply_kernel(test_image, sobel_x_kernel)
  let edge_y = apply_kernel(test_image, sobel_y_kernel)
  
  // 边缘检测应该在中心区域产生高值
  assert_true(edge_x.pixels[2][2] > 0 || edge_y.pixels[2][2] > 0)
}

// 测试3: 图像增强
test "图像增强" {
  // 图像表示
  type Image = {
    width: Int,
    height: Int,
    pixels: Array<Array<Int>>
  }
  
  // 直方图
  type Histogram = Array<Int>  // 256个桶，每个桶存储对应像素值的数量
  
  // 计算直方图
  let calculate_histogram = fn(image: Image) {
    let mut histogram = []
    
    for i in 0..256 {
      histogram = histogram.push(0)
    }
    
    for y in 0..image.height {
      for x in 0..image.width {
        let pixel_value = image.pixels[y][x]
        histogram[pixel_value] = histogram[pixel_value] + 1
      }
    }
    
    histogram
  }
  
  // 直方图均衡化
  let histogram_equalization = fn(image: Image) {
    let histogram = calculate_histogram(image)
    let total_pixels = image.width * image.height
    
    // 计算累积分布函数
    let mut cdf = []
    let mut sum = 0
    
    for i in 0..256 {
      sum = sum + histogram[i]
      cdf = cdf.push(sum)
    }
    
    // 创建查找表
    let mut lookup_table = []
    
    for i in 0..256 {
      let equalized_value = ((cdf[i].to_float() / total_pixels.to_float()) * 255.0).to_int()
      lookup_table = lookup_table.push(equalized_value)
    }
    
    // 应用查找表
    let mut equalized_pixels = []
    
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let pixel_value = image.pixels[y][x]
        row = row.push(lookup_table[pixel_value])
      }
      equalized_pixels = equalized_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: equalized_pixels }
  }
  
  // 线性对比度增强
  let linear_contrast_enhancement = fn(image: Image, min_input: Int, max_input: Int, min_output: Int, max_output: Int) {
    let mut enhanced_pixels = []
    
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let pixel_value = image.pixels[y][x]
        
        let enhanced_value = if pixel_value <= min_input {
          min_output
        } else if pixel_value >= max_input {
          max_output
        } else {
          let ratio = (pixel_value - min_input).to_float() / (max_input - min_input).to_float()
          (min_output.to_float() + ratio * (max_output - min_input).to_float()).to_int()
        }
        
        row = row.push(enhanced_value)
      }
      enhanced_pixels = enhanced_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: enhanced_pixels }
  }
  
  // 伽马校正
  let gamma_correction = fn(image: Image, gamma: Float) {
    let mut corrected_pixels = []
    
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let pixel_value = image.pixels[y][x]
        let normalized_value = pixel_value.to_float() / 255.0
        let corrected_value = (normalized_value.pow(1.0 / gamma) * 255.0).to_int()
        
        row = row.push(corrected_value.max(0).min(255))
      }
      corrected_pixels = corrected_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: corrected_pixels }
  }
  
  // 创建低对比度测试图像
  let create_low_contrast_image = fn() {
    let pixels = [
      [100, 105, 110, 105, 100],
      [105, 110, 115, 110, 105],
      [110, 115, 120, 115, 110],
      [105, 110, 115, 110, 105],
      [100, 105, 110, 105, 100]
    ]
    
    { width: 5, height: 5, pixels }
  }
  
  // 测试直方图计算
  let test_image = create_low_contrast_image()
  let histogram = calculate_histogram(test_image)
  
  // 验证直方图总和等于像素总数
  let mut histogram_sum = 0
  for count in histogram {
    histogram_sum = histogram_sum + count
  }
  assert_eq(histogram_sum, 25)  // 5x5图像
  
  // 测试直方图均衡化
  let equalized_image = histogram_equalization(test_image)
  let equalized_histogram = calculate_histogram(equalized_image)
  
  // 直方图均衡化应该扩展像素值范围
  let original_min = 100
  let original_max = 120
  let equalized_min = 255
  let equalized_max = 0
  
  for i in 0..256 {
    if equalized_histogram[i] > 0 {
      if i < equalized_min {
        equalized_min = i
      }
      if i > equalized_max {
        equalized_max = i
      }
    }
  }
  
  assert_true(equalized_max - equalized_min > original_max - original_min)
  
  // 测试线性对比度增强
  let enhanced_image = linear_contrast_enhancement(test_image, 100, 120, 0, 255)
  
  // 验证增强后的像素值范围
  let mut enhanced_min = 255
  let mut enhanced_max = 0
  
  for y in 0..enhanced_image.height {
    for x in 0..enhanced_image.width {
      let value = enhanced_image.pixels[y][x]
      if value < enhanced_min {
        enhanced_min = value
      }
      if value > enhanced_max {
        enhanced_max = value
      }
    }
  }
  
  assert_eq(enhanced_min, 0)
  assert_eq(enhanced_max, 255)
  
  // 测试伽马校正
  let gamma_2_2 = gamma_correction(test_image, 2.2)  // 标准伽马值
  let gamma_0_5 = gamma_correction(test_image, 0.5)  // 伽马值小于1会使图像变亮
  
  // 伽马2.2应该使图像变暗
  let mut sum_original = 0
  let mut sum_gamma_2_2 = 0
  let mut sum_gamma_0_5 = 0
  
  for y in 0..test_image.height {
    for x in 0..test_image.width {
      sum_original = sum_original + test_image.pixels[y][x]
      sum_gamma_2_2 = sum_gamma_2_2 + gamma_2_2.pixels[y][x]
      sum_gamma_0_5 = sum_gamma_0_5 + gamma_0_5.pixels[y][x]
    }
  }
  
  assert_true(sum_gamma_2_2 < sum_original)  // 图像变暗
  assert_true(sum_gamma_0_5 > sum_original)  // 图像变亮
}

// 测试4: 图像分割
test "图像分割" {
  // 图像表示
  type Image = {
    width: Int,
    height: Int,
    pixels: Array<Array<Int>>
  }
  
  // 区域
  type Region = {
    id: Int,
    pixels: Array<(Int, Int)>  // (x, y)坐标
    mean_value: Float
  }
  
  // 阈值分割
  let threshold_segmentation = fn(image: Image, threshold: Int) {
    let mut segmented_pixels = []
    
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let segmented_value = if image.pixels[y][x] >= threshold {
          255  // 前景
        } else {
          0    // 背景
        }
        row = row.push(segmented_value)
      }
      segmented_pixels = segmented_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: segmented_pixels }
  }
  
  // Otsu阈值法
  let otsu_threshold = fn(image: Image) {
    let histogram = calculate_histogram(image)
    let total_pixels = image.width * image.height
    
    let mut sum = 0
    for i in 0..256 {
      sum = sum + i * histogram[i]
    }
    
    let mut sum_background = 0
    let mut weight_background = 0
    let mut max_variance = 0.0
    let mut optimal_threshold = 0
    
    for threshold in 0..256 {
      weight_background = weight_background + histogram[threshold]
      
      if weight_background == 0 {
        continue
      }
      
      let weight_foreground = total_pixels - weight_background
      
      if weight_foreground == 0 {
        break
      }
      
      sum_background = sum_background + threshold * histogram[threshold]
      
      let mean_background = sum_background.to_float() / weight_background.to_float()
      let mean_foreground = (sum - sum_background).to_float() / weight_foreground.to_float()
      
      let between_class_variance = weight_background.to_float() * weight_foreground.to_float() * 
                                  (mean_background - mean_foreground) * (mean_background - mean_foreground)
      
      if between_class_variance > max_variance {
        max_variance = between_class_variance
        optimal_threshold = threshold
      }
    }
    
    optimal_threshold
  }
  
  // 计算直方图
  let calculate_histogram = fn(image: Image) {
    let mut histogram = []
    
    for i in 0..256 {
      histogram = histogram.push(0)
    }
    
    for y in 0..image.height {
      for x in 0..image.width {
        let pixel_value = image.pixels[y][x]
        histogram[pixel_value] = histogram[pixel_value] + 1
      }
    }
    
    histogram
  }
  
  // 区域生长算法
  let region_growing = fn(image: Image, seed_points: Array<(Int, Int)>, threshold: Int) {
    let mut visited = []
    let mut regions = []
    
    // 初始化访问标记
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        row = row.push(false)
      }
      visited = visited.push(row)
    }
    
    // 对每个种子点进行区域生长
    for (seed_x, seed_y) in seed_points {
      if seed_x >= 0 && seed_x < image.width && seed_y >= 0 && seed_y < image.height && not(visited[seed_y][seed_x]) {
        let mut region_pixels = []
        let mut queue = [(seed_x, seed_y)]
        let seed_value = image.pixels[seed_y][seed_x]
        let mut sum = 0
        
        while queue.length() > 0 {
          let (x, y) = queue[0]
          queue = queue.slice(1)
          
          if x >= 0 && x < image.width && y >= 0 && y < image.height && not(visited[y][x]) {
            let pixel_value = image.pixels[y][x]
            
            if (pixel_value - seed_value).abs() <= threshold {
              visited[y][x] = true
              region_pixels = region_pixels.push((x, y))
              sum = sum + pixel_value
              
              // 添加邻居到队列
              queue = queue.push((x + 1, y))
              queue = queue.push((x - 1, y))
              queue = queue.push((x, y + 1))
              queue = queue.push((x, y - 1))
            }
          }
        }
        
        if region_pixels.length() > 0 {
          let region_id = regions.length()
          let mean_value = sum.to_float() / region_pixels.length().to_float()
          
          regions = regions.push({
            id: region_id,
            pixels: region_pixels,
            mean_value: mean_value
          })
        }
      }
    }
    
    regions
  }
  
  // K-means聚类分割
  let kmeans_segmentation = fn(image: Image, k: Int, max_iterations: Int) {
    let mut centroids = []
    
    // 初始化聚类中心（均匀分布）
    for i in 0..k {
      let value = (255 * i / (k - 1)).to_int()
      centroids = centroids.push(value.to_float())
    }
    
    let mut labels = []
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        row = row.push(0)
      }
      labels = labels.push(row)
    }
    
    let mut iteration = 0
    let mut converged = false
    
    while iteration < max_iterations && not(converged) {
      // 分配像素到最近的聚类中心
      let mut new_labels = []
      
      for y in 0..image.height {
        let mut row = []
        for x in 0..image.width {
          let pixel_value = image.pixels[y][x].to_float()
          
          let mut min_distance = Float::max_value()
          let mut closest_centroid = 0
          
          for i in 0..k {
            let distance = (pixel_value - centroids[i]).abs()
            
            if distance < min_distance {
              min_distance = distance
              closest_centroid = i
            }
          }
          
          row = row.push(closest_centroid)
        }
        new_labels = new_labels.push(row)
      }
      
      // 检查是否收敛
      converged = true
      for y in 0..image.height {
        for x in 0..image.width {
          if labels[y][x] != new_labels[y][x] {
            converged = false
            break
          }
        }
        if not(converged) {
          break
        }
      }
      
      labels = new_labels
      
      // 更新聚类中心
      let mut new_centroids = []
      let mut cluster_sums = []
      let mut cluster_counts = []
      
      for i in 0..k {
        cluster_sums = cluster_sums.push(0.0)
        cluster_counts = cluster_counts.push(0)
      }
      
      for y in 0..image.height {
        for x in 0..image.width {
          let cluster_id = labels[y][x]
          cluster_sums[cluster_id] = cluster_sums[cluster_id] + image.pixels[y][x].to_float()
          cluster_counts[cluster_id] = cluster_counts[cluster_id] + 1
        }
      }
      
      for i in 0..k {
        if cluster_counts[i] > 0 {
          new_centroids = new_centroids.push(cluster_sums[i] / cluster_counts[i].to_float())
        } else {
          new_centroids = new_centroids.push(centroids[i])
        }
      }
      
      centroids = new_centroids
      iteration = iteration + 1
    }
    
    // 创建分割图像
    let mut segmented_pixels = []
    
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let cluster_id = labels[y][x]
        let segmented_value = (centroids[cluster_id]).to_int()
        row = row.push(segmented_value)
      }
      segmented_pixels = segmented_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: segmented_pixels }
  }
  
  // 创建双峰测试图像
  let create_bimodal_test_image = fn() {
    let pixels = [
      [50, 50, 50, 200, 200, 200],
      [50, 50, 50, 200, 200, 200],
      [50, 50, 50, 200, 200, 200],
      [50, 50, 50, 200, 200, 200],
      [50, 50, 50, 200, 200, 200],
      [50, 50, 50, 200, 200, 200]
    ]
    
    { width: 6, height: 6, pixels }
  }
  
  // 测试阈值分割
  let test_image = create_bimodal_test_image()
  let thresholded_image = threshold_segmentation(test_image, 125)
  
  // 验证分割结果
  assert_eq(thresholded_image.pixels[0][0], 0)    // 背景变黑
  assert_eq(thresholded_image.pixels[0][3], 255)  // 前景变白
  
  // 测试Otsu阈值法
  let otsu_threshold_value = otsu_threshold(test_image)
  
  // 对于双峰图像，Otsu阈值应该在两个峰之间
  assert_true(otsu_threshold_value > 50 && otsu_threshold_value < 200)
  
  // 测试区域生长
  let seed_points = [(0, 0), (3, 0)]  // 在两个不同区域设置种子点
  let regions = region_growing(test_image, seed_points, 10)
  
  assert_eq(regions.length(), 2)
  
  // 验证第一个区域（低值区域）
  assert_eq(regions[0].pixels.length(), 18)  // 3x6 = 18
  assert_true(regions[0].mean_value < 100)
  
  // 验证第二个区域（高值区域）
  assert_eq(regions[1].pixels.length(), 18)  // 3x6 = 18
  assert_true(regions[1].mean_value > 100)
  
  // 测试K-means聚类分割
  let kmeans_segmented = kmeans_segmentation(test_image, 2, 10)
  
  // 验证K-means分割结果
  let mut cluster1_found = false
  let mut cluster2_found = false
  
  for y in 0..kmeans_segmented.height {
    for x in 0..kmeans_segmented.width {
      let value = kmeans_segmented.pixels[y][x]
      if value < 100 {
        cluster1_found = true
      } else if value > 100 {
        cluster2_found = true
      }
    }
  }
  
  assert_true(cluster1_found)
  assert_true(cluster2_found)
}

// 测试5: 特征检测
test "特征检测" {
  // 图像表示
  type Image = {
    width: Int,
    height: Int,
    pixels: Array<Array<Int>>
  }
  
  // 特征点
  type FeaturePoint = {
    x: Int,
    y: Int,
    strength: Float
  }
  
  // 计算梯度（Sobel算子）
  let calculate_gradient = fn(image: Image) {
    let sobel_x = [
      [-1, 0, 1],
      [-2, 0, 2],
      [-1, 0, 1]
    ]
    
    let sobel_y = [
      [-1, -2, -1],
      [0, 0, 0],
      [1, 2, 1]
    ]
    
    let mut gradient_x = []
    let mut gradient_y = []
    
    for y in 0..image.height {
      let mut row_x = []
      let mut row_y = []
      
      for x in 0..image.width {
        let mut gx = 0.0
        let mut gy = 0.0
        
        for ky in 0..3 {
          for kx in 0..3 {
            let px = x + kx - 1
            let py = y + ky - 1
            
            let pixel_value = if px >= 0 && px < image.width && py >= 0 && py < image.height {
              image.pixels[py][px].to_float()
            } else {
              0.0
            }
            
            gx = gx + pixel_value * sobel_x[ky][kx].to_float()
            gy = gy + pixel_value * sobel_y[ky][kx].to_float()
          }
        }
        
        row_x = row_x.push(gx)
        row_y = row_y.push(gy)
      }
      
      gradient_x = gradient_x.push(row_x)
      gradient_y = gradient_y.push(row_y)
    }
    
    (gradient_x, gradient_y)
  }
  
  // Harris角点检测
  let harris_corner_detection = fn(image: Image, threshold: Float) {
    let (gradient_x, gradient_y) = calculate_gradient(image)
    let mut corners = []
    
    for y in 1..image.height - 1 {
      for x in 1..image.width - 1 {
        // 计算Harris矩阵
        let mut ixx = 0.0
        let mut ixy = 0.0
        let mut iyy = 0.0
        
        // 使用3x3窗口
        for dy in -1..2 {
          for dx in -1..2 {
            let gx = gradient_x[y + dy][x + dx]
            let gy = gradient_y[y + dy][x + dx]
            
            ixx = ixx + gx * gx
            ixy = ixy + gx * gy
            iyy = iyy + gy * gy
          }
        }
        
        // 计算Harris响应
        let det = ixx * iyy - ixy * ixy
        let trace = ixx + iyy
        let k = 0.04  // 经验值
        let response = det - k * trace * trace
        
        if response > threshold {
          corners = corners.push({
            x: x,
            y: y,
            strength: response
          })
        }
      }
    }
    
    corners
  }
  
  // 非极大值抑制
  let non_maximum_suppression = fn(corners: Array<FeaturePoint>, min_distance: Int) {
    let mut suppressed_corners = []
    
    // 按强度降序排序
    let mut sorted_corners = corners
    let mut i = 0
    while i < sorted_corners.length() - 1 {
      let mut j = i + 1
      while j < sorted_corners.length() {
        if sorted_corners[i].strength < sorted_corners[j].strength {
          let temp = sorted_corners[i]
          sorted_corners[i] = sorted_corners[j]
          sorted_corners[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    // 非极大值抑制
    let mut suppressed = []
    
    for corner in sorted_corners {
      let mut is_suppressed = false
      
      for suppressed_corner in suppressed {
        let distance = ((corner.x - suppressed_corner.x) * (corner.x - suppressed_corner.x) + 
                        (corner.y - suppressed_corner.y) * (corner.y - suppressed_corner.y)).sqrt()
        
        if distance < min_distance.to_float() {
          is_suppressed = true
          break
        }
      }
      
      if not(is_suppressed) {
        suppressed = suppressed.push(corner)
      }
    }
    
    suppressed
  }
  
  // 边缘检测（Canny简化版）
  let canny_edge_detection = fn(image: Image, low_threshold: Float, high_threshold: Float) {
    let (gradient_x, gradient_y) = calculate_gradient(image)
    let mut edge_pixels = []
    
    // 计算梯度幅值和方向
    for y in 0..image.height {
      let mut row = []
      for x in 0..image.width {
        let gx = gradient_x[y][x]
        let gy = gradient_y[y][x]
        let magnitude = (gx * gx + gy * gy).sqrt()
        
        // 简化：直接使用幅值阈值
        let edge_value = if magnitude > high_threshold {
          255  // 强边缘
        } else if magnitude > low_threshold {
          128  // 弱边缘
        } else {
          0    // 非边缘
        }
        
        row = row.push(edge_value)
      }
      edge_pixels = edge_pixels.push(row)
    }
    
    { width: image.width, height: image.height, pixels: edge_pixels }
  }
  
  // 创建角点测试图像
  let create_corner_test_image = fn() {
    let pixels = [
      [100, 100, 100, 100, 100],
      [100, 100, 100, 100, 100],
      [100, 100, 255, 100, 100],
      [100, 100, 100, 100, 100],
      [100, 100, 100, 100, 100]
    ]
    
    { width: 5, height: 5, pixels }
  }
  
  // 创建边缘测试图像
  let create_edge_test_image = fn() {
    let pixels = [
      [100, 100, 100, 100, 100],
      [100, 100, 100, 100, 100],
      [100, 100, 100, 100, 100],
      [200, 200, 200, 200, 200],
      [200, 200, 200, 200, 200]
    ]
    
    { width: 5, height: 5, pixels }
  }
  
  // 测试Harris角点检测
  let corner_test_image = create_corner_test_image()
  let corners = harris_corner_detection(corner_test_image, 1000.0)
  
  // 应该检测到中心像素(2,2)作为角点
  assert_true(corners.length() >= 1)
  
  let center_corner_found = corners.any(fn(corner) { corner.x == 2 && corner.y == 2 })
  assert_true(center_corner_found)
  
  // 测试非极大值抑制
  let suppressed_corners = non_maximum_suppression(corners, 2)
  
  // 非极大值抑制后应该有更少的角点
  assert_true(suppressed_corners.length() <= corners.length())
  
  // 测试边缘检测
  let edge_test_image = create_edge_test_image()
  let edge_image = canny_edge_detection(edge_test_image, 50.0, 100.0)
  
  // 验证边缘检测结果
  let mut edge_found = false
  
  for y in 0..edge_image.height {
    for x in 0..edge_image.width {
      if edge_image.pixels[y][x] > 0 {
        edge_found = true
        break
      }
    }
    if edge_found {
      break
    }
  }
  
  assert_true(edge_found)
}
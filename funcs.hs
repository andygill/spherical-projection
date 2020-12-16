module Funcs where
unFisheyeTransform = (\(height, width, x, y) -> 
    let v35 = 0.5 in
    let v30 = 0.5 in
    let v20 = x / width in
    let v19 = 1.0 in
    let v18 = v19 - v20 in
    let v12 = y / height in
    let v11 = 3.141592653589793 in
    let v17 = v11 * v18 in
    let v16 = cos v17 in
    let v28 = sin v17 in
    let v10 = v11 * v12 in
    let v23 = sin v10 in
    let v15 = v16 * v23 in
    let v29 = sin v10 in
    let v27 = v28 * v29 in
    let v26 = acos v27 in
    let v25 = v26 / v11 in
    let v24 = cos v25 in
    let v34 = sin v25 in
    let v9 = cos v10 in
    let v7 = -1.0 in
    let v6 = v7 * v9 in
    let v5 = atan2 v6 v15 in
    let v33 = v5 * v34 in
    let v32 = v33 + v35 in
    let v31 = v32 * height in
    let v4 = v5 * v24 in
    let v3 = v4 + v30 in
    let v2 = v3 * width in
    let v1 = (v2, v31) in
    v1)

inverseFisheye = (\(height, width, side, aperture, x, y) -> 
    let v53 = 2.0 in
    let v42 = 1.0 in
    let v40 = 2.0 in
    let v38 = height / v40 in
    let v36 = 3.141592653589793 in
    let v31 = 2.0 in
    let v29 = 2.0 in
    let v28 = side / v29 in
    let v26 = y / v28 in
    let v25 = 1.0 in
    let v24 = v25 - v26 in
    let v23 = v24 * v24 in
    let v22 = 1.0 in
    let v21 = 2.0 in
    let v19 = side / v21 in
    let v17 = x / v19 in
    let v16 = v17 - v22 in
    let v35 = atan2 v24 v16 in
    let v34 = cos v35 in
    let v48 = sin v35 in
    let v15 = v16 * v16 in
    let v14 = v15 + v23 in
    let v13 = sqrt v14 in
    let v12 = v13 * aperture in
    let v11 = v12 / v31 in
    let v33 = sin v11 in
    let v32 = v33 * v34 in
    let v51 = v32 ^ 2 in
    let v47 = sin v11 in
    let v46 = v47 * v48 in
    let v10 = cos v11 in
    let v52 = v10 ^ 2 in
    let v50 = v51 + v52 in
    let v49 = sqrt v50 in
    let v45 = atan2 v46 v49 in
    let v44 = v45 * v53 in
    let v43 = v44 / v36 in
    let v41 = v42 - v43 in
    let v37 = v38 * v41 in
    let v9 = atan2 v10 v32 in
    let v8 = v9 / v36 in
    let v7 = 1.0 in
    let v6 = v7 + v8 in
    let v5 = 2.0 in
    let v3 = width / v5 in
    let v2 = v3 * v6 in
    let v1 = (v2, v37) in
    v1)


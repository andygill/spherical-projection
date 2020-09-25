module Test1 where
unFisheyeTransform = (\(v18, v4, v36, v31, v16) ->
    let v42 = 1.0 in
    let v40 = 2.0 in
    let v39 = v18 / v40 in
    let v35 = 2.0 in
    let v34 = 1.0 in
    let v33 = 2.0 in
    let v32 = v4 / v33 in
    let v30 = v31 / v32 in
    let v29 = v30 - v34 in
    let v21 = 2.0 in
    let v20 = 3.141592653589793 in
    let v28 = v29 * v20 in
    let v27 = sin v28 in
    let v47 = cos v28 in
    let v19 = 2.0 in
    let v17 = v18 / v19 in
    let v15 = v16 / v17 in
    let v14 = 1.0 in
    let v13 = v14 - v15 in
    let v12 = v13 * v20 in
    let v11 = v12 / v21 in
    let v26 = cos v11 in
    let v25 = v26 * v27 in
    let v24 = acos v25 in
    let v23 = v24 * v35 in
    let v22 = v23 / v36 in
    let v37 = sin v24 in
    let v48 = sin v24 in
    let v46 = cos v11 in
    let v45 = v46 * v47 in
    let v44 = v45 * v22 in
    let v43 = v44 / v48 in
    let v41 = v42 - v43 in
    let v38 = v39 * v41 in
    let v10 = sin v11 in
    let v9 = v10 * v22 in
    let v8 = v9 / v37 in
    let v7 = 1.0 in
    let v6 = v7 + v8 in
    let v5 = 2.0 in
    let v3 = v4 / v5 in
    let v2 = v3 * v6 in
    let v1 = (v2, v38) in
    v1)

inverseFisheye = (\(v39, v4, v20, v30, v18, v27) ->
    let v53 = 2.0 in
    let v42 = 1.0 in
    let v40 = 2.0 in
    let v38 = v39 / v40 in
    let v36 = 3.141592653589793 in
    let v31 = 2.0 in
    let v29 = 2.0 in
    let v28 = v20 / v29 in
    let v26 = v27 / v28 in
    let v25 = 1.0 in
    let v24 = v25 - v26 in
    let v23 = v24 * v24 in
    let v22 = 1.0 in
    let v21 = 2.0 in
    let v19 = v20 / v21 in
    let v17 = v18 / v19 in
    let v16 = v17 - v22 in
    let v35 = atan2 v24 v16 in
    let v34 = cos v35 in
    let v48 = sin v35 in
    let v15 = v16 * v16 in
    let v14 = v15 + v23 in
    let v13 = sqrt v14 in
    let v12 = v13 * v30 in
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
    let v3 = v4 / v5 in
    let v2 = v3 * v6 in
    let v1 = (v2, v37) in
    v1)
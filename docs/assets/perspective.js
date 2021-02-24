

var renderer = new THREE.WebGLRenderer();
renderer.setSize( window.innerWidth, window.innerHeight );
document.querySelector('#c').appendChild( renderer.domElement );

var scene = new THREE.Scene();
var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );

const controls = new OrbitControls( camera, renderer.domElement );

//controls.update() must be called after any manual changes to the camera's transform
camera.position.set( 0, 20, 10 );
controls.update();

var loader= new THREE.TextureLoader();
var texture = loader.load("https://i.imgur.com/y9eyLnn.jpg");
const material = new THREE.MeshBasicMaterial({ map: texture });

const sphereGeo = new THREE.SphereGeometry(1, 32, 32);
const sphere = new THREE.Mesh(sphereGeo, material);
scene.add( sphere );

/*const sphereCopy = sphere.clone();

let arr = [],
	x = [];
let s = new THREE.Spherical();
for (v in sphereCopy.geometry.vertices){
	s.setFromVector3(v.sub(sphereCopy.position));
	x = sphere2planeStereo([s.phi, s.theta]);
	arr.push(new THREE.Vector3(x[0], x[1], 1));
}
sphereCopy.geometry.setFromPoints(arr);
sphereCopy.translateX(-2);
scene.add(sphereCopy);*/

const planeGeo = new THREE.PlaneGeometry(4, 2, 30, 30);
const plane = new THREE.Mesh(planeGeo, material);
plane.translateX(3);
scene.add(plane);
console.log(plane);

camera.position.z = 3;

var animate = function () {
	requestAnimationFrame( animate );

	sphere.rotation.x += 0.01;
	sphere.rotation.y += 0.01;

	renderer.render( scene, camera );
};

animate();

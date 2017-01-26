$(document).ready(function() {

	var exp = 1
	var noise = 'impulse-local'
	$('#exp1').css('background-color', '#bdf')
	$('#impulse-local').css('background-color', '#bdf')

	var fricVal = 0.5
	var fricVar = 0

	game = document.getElementById('game-frame')
	$('#exp2').click(function() {
		$('#exp2').css('background-color', '#bdf')
		$('#exp1').css('background-color', '#eee')
		exp = 2
	})
	$('#exp1').click(function() {
		$('#exp1').css('background-color', '#bdf')
		$('#exp2').css('background-color', '#eee')
		exp = 1
	})
	$('#impulse-local').click(function() {
		$('#impulse-local').css('background-color', '#bdf')
		$('#impulse-global').css('background-color', '#eee')
		$('#impulse-local-above-extended').css('background-color', '#eee')
		noise = 'impulse-local'
	})
	$('#impulse-global').click(function() {
		$('#impulse-local').css('background-color', '#eee')
		$('#impulse-global').css('background-color', '#bdf')
		$('#impulse-local-above-extended').css('background-color', '#eee')
		noise = 'impulse-global'
	})
	$('#impulse-local-above-extended').click(function() {
		$('#impulse-local').css('background-color', '#eee')
		$('#impulse-global').css('background-color', '#eee')
		$('#impulse-local-above-extended').css('background-color', '#bdf')
		noise = 'impulse-local-above-extended'
	})

	$('#load').click(function() {
		setNoiseParams();
		game.contentWindow.loadClipNoAnimate(exp+'_'+$('#trial-num').val()); 
	})

	$('#load-remove').click(function() {
		setNoiseParams();
		game.contentWindow.loadRemoveClip(exp+'_'+$('#trial-num').val()); 
	})

	function setNoiseParams() {
		game.contentWindow.setNoiseParams(noise, $('#noise-level').val(), fricVal, fricVar)
		game.contentWindow.setTableFriction((exp === 1) ? 1 : 0.5);
	}

	function removeBrick() {
		game.contentWindow.removeBrick(noise, $('#noise-level').val(), fricVal, fricVar)
	}

	$('#noise-level').on("change", function() {
		$('#level-num').text($('#noise-level').val());
	});

	document.addEventListener('keypress', function (e) {
		return;
		if (e.charCode === 108) {
			game.contentWindow.loadClip(exp+'_'+$('#trial-num').val()); 
		} else if (e.charCode === 115) {
			var canvas = game.contentWindow.document.getElementById("c"); 
			canvas.toBlob(function(blob){
				saveAs(blob,"screenshot.png");
			}, "image/png;base64");
		} else if (e.charCode === 100) {
			game.contentWindow.saveOneData();
		}
	})
})

function minmax(value, min, max) 
{
	value = parseInt(value)
    if(value < min || isNaN(value)) 
        return 1; 
    else if(value > max) 
        return 42; 
    else return value;
}

function start(type) {
	setNoiseParams();
	if (type == 'animate') {
		document.getElementById('game-frame').contentWindow.animate();
	} else if (type == 'high') {
		document.getElementById('game-frame').contentWindow.animateHigh();
	} else {
		document.getElementById('game-frame').contentWindow.sample(
			document.getElementById("numSamples").value,
			document.getElementById("numRepetitions").value
		);
	}
}

function saveLoad() {
	var x = document.getElementById("idnums").value.split(/[ ,]+/);
	document.getElementById('game-frame').contentWindow.loadAndSave(x);
}

function clickBricks() {
	document.getElementById('game-frame').contentWindow.clickBricks();
}
function changeGravity() {
	document.getElementById('game-frame').contentWindow.changeGravity(
		document.getElementById('grav').checked
	);
}
function changeAngle() {
	document.getElementById('game-frame').contentWindow.changeAngle(
		document.getElementById('brickAngle').value
	)
}
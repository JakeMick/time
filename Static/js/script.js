var game = new Phaser.Game(400, 490, Phaser.AUTO, 'game');

var main_state = {

    preload: function() { 
        this.game.stage.backgroundColor = '#71c5cf';
        this.game.load.image('bird', 'img/bird.png');
    },

    create: function() { 
        this.bird = this.game.add.sprite(100, 245, 'bird');
        this.bird.body.gravity.y = 1000;
        var space_key = this.game.input.keyboard.addKey(Phaser.Keyboard.SPACEBAR);
        space_key.onDown.add(this.jump, this)
    },
    
    update: function() {
        if (this.bird.inWorld === false) {
            this.restart_game();
        }
    },
};

game.state.add('main', main_state);  
game.state.start('main'); 

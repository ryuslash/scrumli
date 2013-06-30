/** @jsx React.DOM */
var StateIcon = React.createClass({
    render: function() {
        var icon_names = {"TODO": "icon-check-empty",
                          "DOING": "icon-sign-blank",
                          "DONE": "icon-check"};
        return <i class={icon_names[this.props.state]}></i>;
    }
});

var StoryData = React.createClass({
    render: function() {
        if (this.props.data) {
            return (<div>
                    Assignee: {this.props.data.assignee}
                    <pre>{this.props.data.content}</pre>
                    </div>);
        }

        return null;
    }
});

var StoryRow = React.createClass({
    render: function() {
        // A little ugly to get a space, but I don't know of any other
        // way.
        var state = " " + this.state.state;
        var sdata = null;

        if (this.state.content)
            sdata = <StoryData data={this.state.content} />;

        return (
            <tr>
              <td>
                <i class="icon-arrow-up" onClick={this.moveUp}></i>
                <i class="icon-arrow-down" onClick={this.moveDown}></i>
              </td>
              <td>
                <span onClick={this.changeState}>
                  <StateIcon state={this.state.state} />
                  {state}
                </span>
              </td>
              <td>
                <a onClick={this.handleClick}>
                  As a {this.props.story.role}, I
                  {this.props.story.necessity} to
                  {this.props.story.title}
                </a>
                <br />
                {sdata}
              </td>
            </tr>
        );
    },
    getInitialState: function() {
        return {state: this.props.story.state,
                content: null};
    },
    handleClick: React.autoBind(function(event) {
        if (!!this.state.content) {
            this.setState({content: null});
            return;
        }
        var self = this;

        $.get('/stories/' + this.props.story.id, null,
              function (data, textStatus, jqXHR) {
                  self.setState({content: data});
              }, 'json');
    }),
    changeState: React.autoBind(function(event) {
        $.ajax({
            url: "/stories/state",
            type: "POST",
            data: {'id': this.props.story.id},
            dataType: 'json',
            mimeType: 'textPlain',
            success: function(data) {
                this.setState({state: eval(data).state});
            }.bind(this)
        });
    }),
    moveUp: React.autoBind(function(event) {
        $.ajax({
            url: "/stories/up",
            type: "POST",
            data: {'id': this.props.story.id},
            dataType: 'json',
            mimeType: 'textPlain',
            success: function (data) {
                this.props.onMoved(1);
            }.bind(this)
        });
    }),
    moveDown: React.autoBind(function(event) {
        $.ajax({
            url: "/stories/down",
            type: "POST",
            data: {'id': this.props.story.id},
            dataType: 'json',
            mimeType: 'textPlain',
            success: function (data) {
                this.props.onMoved(-1);
            }.bind(this)
        });
    })
});

var StoryTable = React.createClass({
    loadStoriesFromServer: function() {
        $.ajax({
            url: this.props.url,
            mimeType: 'textPlain',
            success: function(data) {
                this.setState({data: eval(data)});
            }.bind(this)
        });
    },
    getInitialState: function() {
        return {data: []};
    },
    componentWillMount: function() {
        this.loadStoriesFromServer();
        setInterval(
            this.loadStoriesFromServer.bind(this),
            this.props.pollInterval
        );
    },
    handleMoved: React.autoBind(function(direction) {
        this.loadStoriesFromServer();
    }),
    render: function() {
        var storyNodes = this.state.data.map(function (story) {
            return <StoryRow story={story} onMoved={this.handleMoved} />;
        }.bind(this));
        return (
            <table class="table table-striped">
              {storyNodes}
            </table>
        );
    }
});

var StoryForm = React.createClass({
    handleSubmit: React.autoBind(function() {
        var role = this.refs.role.getDOMNode().value.trim();
        var necessity = this.refs.necessity.getDOMNode().value.trim();
        var headline = this.refs.headline.getDOMNode().value.trim();
        var content = this.refs.content.getDOMNode().value.trim();

        this.props.onStorySubmit({role: role,
                                  necessity: necessity,
                                  headline: headline,
                                  content: content});

        this.refs.role.getDOMNode().value = '';
        this.refs.necessity.getDOMNode().value = '';
        this.refs.headline.getDOMNode().value = '';
        this.refs.content.getDOMNode().value = '';

        return false;
    }),
    render: function() {
        return (
            <form onSubmit={this.handleSubmit}>
              <fieldset>
                <legend class="toggle">New story</legend>
                <div id="new-story">
                  <div class="input-prepend input-append">
                    <span class="add-on">As a </span>
                    <input type="text" class="input-medium" ref="role"
                           placeholder="person" />
                    <span class="add-on"> I </span>
                    <input type="text" class="input-small"
                           ref="necessity" placeholder="would like" />
                    <span class="add-on"> to </span>
                    <input type="text" class="input-xxlarge"
                           ref="headline" placeholder="fill in this form..." />
                    <button class="btn" type="submit">!</button><br />
                    <textarea ref="content"></textarea>
                  </div>
                </div>
              </fieldset>
            </form>
        );
    }
});

var StoryPage = React.createClass({
    handleStorySubmit: React.autoBind(function (story) {
        $.ajax({
            url: "/stories/new",
            type: "POST",
            data: story,
            dataType: 'json',
            mimeType: 'textPlain'
        });
    }),
    render: function() {
        return (
            <div>
              <StoryTable url="/stories" pollInterval={5000} />
              <StoryForm onStorySubmit={this.handleStorySubmit} />
            </div>
        );
    }
});

React.renderComponent(
    <StoryPage />,
    document.getElementById('content')
);

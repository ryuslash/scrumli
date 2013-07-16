/** @jsx React.DOM */
var StateIcon = React.createClass({
    render: function() {
        var icon_names = {"TODO": "icon-check-empty",
                          "DOING": "icon-sign-blank",
                          "DONE": "icon-check"};
        return <i class={icon_names[this.props.state]}></i>;
    }
});

var StoryTaskRow = React.createClass({
    changeState: React.autoBind(function(event) {
        $.post("/tasks/state", {'id': this.props.task.id})
            .done(function (data, textStatus, jqXHR) {
                if (data.status == "ok")
                    this.setState({state: data.state});
            }.bind(this));
    }),
    getInitialState: function () {
        return {state: this.props.task.state};
    },
    moveUp: React.autoBind(function(event) {
        $.post("/tasks/up", {'id': this.props.task.id})
            .done(function (data) {
                if (data.status == "ok")
                    this.props.onMoved(1);
            }.bind(this));
    }),
    moveDown: React.autoBind(function(event) {
        $.post("/tasks/down", {'id': this.props.task.id})
            .done(function (data) {
                if (data.status == "ok")
                    this.props.onMoved(-1);
            }.bind(this));
    }),
    render: function() {
        return (
            <tr>
              <td class="span1">
                <i class="icon-arrow-up clickable"
                   onClick={this.moveUp}></i>
                <i class="icon-arrow-down clickable"
                   onClick={this.moveDown}></i>
              </td>
              <td class="span2">
                <span onClick={this.changeState} class="clickable">
                  <StateIcon state={this.state.state} /> {" "}
                  {this.state.state}
                </span>
              </td>
              <td>
                {this.props.task.description}
              </td>
            </tr>
        );

    }
});

var StoryTaskTable = React.createClass({
    render: function() {
        var taskNodes = this.props.tasks.map(function (task) {
            return <StoryTaskRow task={task}
                                 onMoved={this.props.onTaskMoved} />;
        }.bind(this));

        return (
            <table class="table table-striped">
              {taskNodes}
            </table>
        );
    }
});

var StoryTaskForm = React.createClass({
    handleSubmit: React.autoBind(function() {
        var text = this.refs.text.getDOMNode().value.trim();

        this.props.onTaskSubmit({description: text});

        this.refs.text.getDOMNode().value = '';

        return false;
    }),
    render: function() {
        return (
            <form onSubmit={this.handleSubmit}>
              <fieldset>
                <legend>New task</legend>
                <div class="input-append">
                  <input type="text" ref="text" class="input-medium" />
                  <button type="submit" class="btn btn-primary">
                    Send
                  </button>
                </div>
              </fieldset>
            </form>
        );
    }
});

var StoryData = React.createClass({
    handleTaskSubmit: React.autoBind(function (task) {
        task.storyId = this.state.data.id;
        $.post("/stories/tasks/new", task)
            .done(function(data) {
                if (data.status == "ok")
                    this.loadStoryFromServer();
            }.bind(this));
    }),
    loadStoryFromServer: function() {
        $.get("/stories/" + this.state.data.id)
            .done(this.setData.bind(this));
    },
    componentWillMount: function() {
        setInterval(
            this.loadStoryFromServer.bind(this),
            this.props.pollInterval
        );
    },
    getInitialState: function() {
        return {data: null};
    },
    setData: function(data) {
        this.setState({data: null});
        this.setState({data: data});
    },
    handleTaskMoved: React.autoBind(function(direction) {
        this.loadStoryFromServer();
    }),
    render: function() {
        if (this.state.data) {
            return (<div>
                      <h1>{this.state.data.title}</h1>
                      Assignee: {this.state.data.assignee}
                      <div class="well normalText">
                        {this.state.data.content}
                      </div>
                      <StoryTaskTable tasks={this.state.data.tasks || []}
                                      onTaskMoved={this.handleTaskMoved} />
                      <StoryTaskForm onTaskSubmit={this.handleTaskSubmit} />
                    </div>);
        }

        return <div></div>;
    }
});

var StoryRow = React.createClass({
    render: function() {
        return (
            <tr>
              <td class="span1">
                <i class="icon-arrow-up clickable"
                   onClick={this.moveUp}></i>
                <i class="icon-arrow-down clickable"
                   onClick={this.moveDown}></i>
              </td>
              <td class="span2">
                <span onClick={this.changeState} class="clickable">
                  <StateIcon state={this.state.state} /> {" "}
                  {this.state.state}
                </span>
              </td>
              <td>
                <a onClick={this.handleClick} class="clickable">
                  As a {this.props.story.role}, I
                  {this.props.story.necessity} to
                  {this.props.story.title}
                </a>
              </td>
            </tr>
        );
    },
    getInitialState: function() {
        return {state: this.props.story.state,
                content: null};
    },
    handleClick: React.autoBind(function(event) {
        this.props.onTitleClicked(this.props.story.id);
    }),
    changeState: React.autoBind(function(event) {
        $.post("/stories/state", {'id': this.props.story.id})
            .done(function(data, textStatus, jqXHR) {
                if (data.status == "ok")
                    this.setState({state: data.state});
            }.bind(this));
    }),
    moveUp: React.autoBind(function(event) {
        $.post("/stories/up", {'id': this.props.story.id})
            .done(function (data, textStatus, jqXHR) {
                if (data.status == "ok")
                    this.props.onMoved(1);
            }.bind(this));
    }),
    moveDown: React.autoBind(function(event) {
        $.post("/stories/down", {'id': this.props.story.id})
            .done(function (data) {
                if (data.status == "ok")
                    this.props.onMoved(-1);
            }.bind(this));
    })
});

var StoryTable = React.createClass({
    handleMoved: React.autoBind(function(direction) {
        this.props.onStoryMoved(direction);
    }),
    handleSelected: React.autoBind(function(storyId) {
        this.props.onStorySelected(storyId);
    }),
    render: function() {
        var storyNodes = this.props.data.map(function (story) {
            return <StoryRow story={story} onMoved={this.handleMoved}
                             onTitleClicked={this.handleSelected} />;
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

        $(".myModal").modal('hide');
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
          <div class="myModal modal fade hide">
            <form onSubmit={this.handleSubmit} class="form-horizontal">
              <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal">
                  &times;
                </button>
                <h3 id="myModalLabel">New story</h3>
              </div>
              <div class="modal-body">
                <div id="new-story">
                  <div class="control-group">
                    <label class="control-label">As a</label>
                    <div class="controls">
                      <input type="text" ref="role" placeholder="person" />
                    </div>
                  </div>
                  <div class="control-group">
                    <label class="control-label">I</label>
                    <div class="controls">
                      <input type="text" ref="necessity"
                             placeholder="would like" />
                    </div>
                  </div>
                  <div class="control-group">
                    <label class="control-label">to</label>
                    <div class="controls">
                      <input type="text" ref="headline"
                             placeholder="fill in this form..." />
                    </div>
                  </div>
                  <div class="control-group">
                    <div class="controls">
                      <textarea ref="content"></textarea>
                    </div>
                  </div>
                </div>
              </div>
              <div class="modal-footer">
                <button class="btn" data-dismiss="modal" aria-hidden="true">
                  Close
                </button>
                <button class="btn btn-primary" type="submit">Save</button>
              </div>
            </form>
          </div>
        );
    }
});

var StoryPage = React.createClass({
    loadStoriesFromServer: function() {
        $.get(this.props.url)
            .done(function(data) {
                this.setState({data: []});
                this.setState({data: data});
            }.bind(this));
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
    handleStoryMoved: React.autoBind(function (direction) {
        this.loadStoriesFromServer();
    }),
    handleStorySubmit: React.autoBind(function (story) {
        $.post("/stories/new", story)
            .done(function (data, textStatus, jqXHR) {
                if (data.status == "ok")
                    this.loadStoriesFromServer();
            }.bind(this))
            .fail(function (jqXHR, textStatus, errorThrown) {
                alert("error: " + errorThrown);
            }.bind(this));
    }),
    handleStorySelected: React.autoBind(function (storyId) {
        $.get('/stories/' + storyId)
            .done(function (data, textStatus, jqXHR) {
                this.refs.data.setData(data);
            }.bind(this), 'json');
    }),
    render: function() {
        return (
            <div class="row">
              <div class="span6">
                <StoryTable data={this.state.data}
                            onStoryMoved={this.handleStoryMoved}
                            onStorySelected={this.handleStorySelected} />
                <StoryForm onStorySubmit={this.handleStorySubmit} />
              </div>
              <div class="span6">
                <StoryData ref="data"
                           pollInterval={this.props.pollInterval} />
              </div>
            </div>
        );
    }
});

React.renderComponent(
    <StoryPage url="/stories" pollInterval={5000} />,
    document.getElementById('content')
);

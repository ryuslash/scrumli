/** @jsx React.DOM */
/* scrumli --- A simple scrum web application
   Copyright (C) 2013  Tom Willemse

   scrumli is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   scrumli is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with scrumli.  If not, see <http://www.gnu.org/licenses/>. */

var StateIcon = React.createClass({
    render: function() {
        var icon_names = {"TODO": "icon-check-empty",
                          "DOING": "icon-sign-blank",
                          "DONE": "icon-check"};
        return <i class={icon_names[this.props.state]}></i>;
    }
});

var AssigneeIcon = React.createClass({
    render: function() {
        var icon;

        if (this.props.assignee)
            icon = <img src={"https://secure.gravatar.com/avatar/" +
                             this.props.md5 + '?s=24'}
                        title={this.props.assignee}
                        alt={this.props.assignee} />;
        else
            icon = <i title="Unknown"
                      class="icon-question icon-border"></i>;

        return icon;
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
    handleAssigneeClick: React.autoBind(function(event) {
        this.props.onAssigneeClicked({url: "/tasks/assignee",
                                      id: this.props.task.id,
                                      assignee: this.props.task.assignee,
                                      md5: this.props.task.md5});
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
              <td class="span1">
                <button onClick={this.handleAssigneeClick}
                        class="nothing">
                  <AssigneeIcon assignee={this.props.task.assignee}
                                md5={this.props.task.md5} />
                </button>
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
                                 onMoved={this.props.onTaskMoved}
                                 onAssigneeClicked={this.props.onAssigneeClicked} />;
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
                      <div class="well normalText">
                        {this.state.data.content}
                      </div>
                      <StoryTaskTable tasks={this.state.data.tasks || []}
                                      onTaskMoved={this.handleTaskMoved}
                                      onAssigneeClicked={this.props.onAssigneeClicked} />
                      <StoryTaskForm onTaskSubmit={this.handleTaskSubmit} />
                    </div>);
        }

        return <div></div>;
    }
});

var StoryRow = React.createClass({
    handleAssigneeClick: React.autoBind(function(event) {
        this.props.onAssigneeClicked({url: "/story/assignee",
                                      id: this.props.story.id,
                                      assignee: this.props.story.assignee,
                                      md5: this.props.story.md5});
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
              <td class="span1">
                <button onClick={this.handleAssigneeClick}
                        class="nothing">
                  <AssigneeIcon assignee={this.props.story.assignee}
                                md5={this.props.story.md5} />
                </button>
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
    render: function() {
        var storyNodes = this.props.data.map(function (story) {
            return <StoryRow story={story} onMoved={this.props.onStoryMoved}
                             onTitleClicked={this.props.onStorySelected}
                             onAssigneeClicked={this.props.onAssigneeClicked} />;
        }.bind(this));
        return (
            <table class="table table-striped">
              {storyNodes}
            </table>
        );
    }
});

var AssignmentForm = React.createClass({
    handleChanged: React.autoBind(function() {
        var assignee = this.refs.assignee.getDOMNode().value;

        $.post(this.state.url,
               {id: this.state.id, assignee: assignee})
            .done(function (data) {
                if (data.status == "ok") {
                    this.refs.assignee.getDOMNode().value = '';
                    $(".assignModal").modal('hide');
                }
            }.bind(this));
    }),
    setInfo: React.autoBind(function(info) {
        this.setState(info);
    }),
    getInitialState: function () {
        return {id: 0, assignee: "", url: "", md5: ""};
    },
    render: function() {
        return (
            <div class="assignModal modal fade hide">
                <div class="modal-header">
                  <button type="button" class="close"
                          data-dismiss="modal">
                    &times;
                  </button>
                <h3 id="assignModalLabel">Assign</h3>
              </div>
              <div class="modal-body">
                <AssigneeIcon assignee={this.state.assignee}
                              md5={this.state.md5} /> {" "}
                <input type="text" ref="assignee"
                       value={this.state.assignee}
                       onChange={this.handleChanged} />
              </div>
              <div class="modal-footer">
              </div>
            </div>
        );
    }
});

var StoryForm = React.createClass({
    handleSubmit: React.autoBind(function() {
        var role = this.refs.role.getDOMNode().value.trim();
        var necessity = this.refs.necessity.getDOMNode().value.trim();
        var headline = this.refs.headline.getDOMNode().value.trim();
        var content = this.refs.content.getDOMNode().value.trim();

        $(".newTaskModal").modal('hide');
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
          <div class="newTaskModal modal fade hide">
            <form onSubmit={this.handleSubmit} class="form-horizontal">
              <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal">
                  &times;
                </button>
                <h3 id="newTaskModalLabel">New story</h3>
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
        $.get(this.state.url)
            .done(function(data) {
                this.setState({data: []});
                this.setState({data: data});
            }.bind(this));
    },
    getInitialState: function() {
        return {data: [], url: this.props.url};
    },
    setUrl: React.autoBind(function(url) {
        this.setState({url: url});
    }),
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
    handleAssigneeClicked: React.autoBind(function (info) {
        var form = this.refs.assignmentForm;

        form.setInfo(info);

        $(".assignModal").modal();
    }),
    render: function() {
        return (
            <div class="row">
              <div class="span6">
                <StoryTable data={this.state.data}
                            onStoryMoved={this.handleStoryMoved}
                            onStorySelected={this.handleStorySelected}
                            onAssigneeClicked={this.handleAssigneeClicked} />
                <StoryForm onStorySubmit={this.handleStorySubmit} />
                <AssignmentForm ref="assignmentForm" />
              </div>
              <div class="span6">
                <StoryData ref="data"
                           pollInterval={this.props.pollInterval}
                           onAssigneeClicked={this.handleAssigneeClicked} />
              </div>
            </div>
        );
    }
});

var StoryFilter = React.createClass({
    getInitialState: function() {
        return {filter: 'all'};
    },
    handleClick: React.autoBind(function(event) {
        this.setState({filter: (this.state.filter != 'all'
                                ? 'all' : 'user')});
        scrumli_page.setUrl((this.state.filter == "all"
                             ? "/stories" : "/stories/mine"));
    }),
    render: function() {
        var classes = {all: ['icon-group', 'All'],
                       user: ['icon-user', 'Mine']};

        return (<a onClick={this.handleClick}>
                  <i class={classes[this.state.filter][0] + ' icon-light'}></i>
                  {" "} {classes[this.state.filter][1]}
                </a>);
    }
});

var scrumli_page = <StoryPage url="/stories" pollInterval={5000} />;

React.renderComponent(
    scrumli_page,
    document.getElementById('content')
);

React.renderComponent(
    <StoryFilter />,
    document.getElementById('filter')
);

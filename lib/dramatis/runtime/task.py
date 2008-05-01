class Task(object):

    @property
    def type(self):
        return self.dest

    @property
    def method(self):
        return self.args[0]

    @property
    def arguments(self):
        return self.args[1:]

    def __init__(self, actor, dest, args, options ):
        self.actor = actor
        self.dest = dest
        self.args = tuple(args)

        self.call_thread = None

        '''
        name = Dramatis::Runtime::Scheduler.actor
        actor = name.instance_eval { self.actor }

        object_id = actor.object.object_id

        self.args.each_with_index do |arg,i|
        if arg.object_id == object_id
        self.args[i] = name
        end
        end

        if actor.call_threading?:
            # warn "oct #{options[:call_thread]} act #{actor.call_thread}"
            if options[:call_thread] and \
                             actor.call_thread and \
                             options[:call_thread] != actor.call_thread:
              raise "hell"
            self.call_thread = actor.call_thread
        if self.call_thread == nil
        self.call_thread = self.to_s

        # warn "task #{self} #{args[0]} call thread [ #{self.call_thread} ] #{options.to_a.join(' ')}"

        case options[:continuation]
        when :none
        self.continuation = Continuation::None.new name, self.call_thread
        when :rpc
        self.continuation = Continuation::RPC.new name, self.call_thread
        when :future
        self.continuation = Continuation::Future.new name, self.call_thread
        when Proc
        self.continuation = Continuation::Proc.new name,  self.call_thread, options[:continuation], \
                                                                             options[:exception]
        else
        raise Dramatis::Internal.new( "invalid contiunation type" )
        '''

    def queued(self):
        self.continuation.queued()

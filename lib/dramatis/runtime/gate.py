class Gate(object):

    def refuse(self,*args):
        pass

    def always(self,args,value,options={}):
        pass

    def accepts(self,*args):
        '''
        accepted = nil
      ( @always + @list ).each do |entry|
        vector, result = entry
        matches = true
        vector.each_with_index do |v, i|
          # warn "#{v} #{args[i]} => #{v === args[i]} so #{result}"
          if not( v === args[i] )
            matches = false
            break
          end
        end
        if matches
          accepted = result
          break
        end
      end
      # warn "last '#{accepted}'"
      # p "accepts? #{args.join(" ")} => '#{accepted}'"
      accepted == true
      '''
        return False

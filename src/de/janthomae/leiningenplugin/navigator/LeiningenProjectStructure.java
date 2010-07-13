package de.janthomae.leiningenplugin.navigator;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.treeStructure.SimpleNode;
import com.intellij.ui.treeStructure.SimpleTree;
import com.intellij.ui.treeStructure.SimpleTreeBuilder;
import com.intellij.ui.treeStructure.SimpleTreeStructure;
import de.janthomae.leiningenplugin.project.LeiningenProject;
import de.janthomae.leiningenplugin.project.LeiningenProjectsManager;

import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import java.util.ArrayList;
import java.util.List;

/**
 * @author <a href="janthomae@janthomae.de">Jan Thom&auml;</a>
 * @version $Id:$
 */
public class LeiningenProjectStructure extends SimpleTreeStructure {

    private SimpleTreeBuilder myTreeBuilder;
    private RootNode myRoot = new RootNode(null);
    private Project myProject;
    private final LeiningenProjectsManager myProjectsManager;

    public LeiningenProjectStructure(Project project, LeiningenProjectsManager projectsManager, SimpleTree tree) {
        super();
        myProject = project;
        myProjectsManager = projectsManager;
        myTreeBuilder = new SimpleTreeBuilder(tree, (DefaultTreeModel) tree.getModel(), this, null);
        Disposer.register(myProject, myTreeBuilder);

        myTreeBuilder.initRoot();
        myTreeBuilder.expand(myRoot, null);

    }

    @Override
    public Object getRootElement() {
        return myRoot;
    }

    public void update() {
        myRoot.clear();
        final List<LeiningenProject> projects = myProjectsManager.getLeiningenProjects();
        for (LeiningenProject project : projects) {
            LeiningenProjectNode lpn = new LeiningenProjectNode(myRoot, project);
            myRoot.addProjectNode(lpn);
        }
        myTreeBuilder.updateFromRoot();
        myTreeBuilder.expand(myRoot, null);
    }


    public static <T extends SimpleNode> List<T> getSelectedNodes(SimpleTree tree, Class<T> nodeClass) {
        final List<T> filtered = new ArrayList<T>();
        for (SimpleNode node : getSelectedNodes(tree)) {
            if ((nodeClass != null) && (!nodeClass.isInstance(node))) {
                filtered.clear();
                break;
            }
            //noinspection unchecked
            filtered.add((T) node);
        }
        return filtered;
    }

    private static List<SimpleNode> getSelectedNodes(SimpleTree tree) {
        List<SimpleNode> nodes = new ArrayList<SimpleNode>();
        TreePath[] treePaths = tree.getSelectionPaths();
        if (treePaths != null) {
            for (TreePath treePath : treePaths) {
                nodes.add(tree.getNodeFor(treePath));
            }
        }
        return nodes;
    }

}

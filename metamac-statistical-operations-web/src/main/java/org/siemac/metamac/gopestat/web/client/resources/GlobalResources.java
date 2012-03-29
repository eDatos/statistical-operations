package org.siemac.metamac.gopestat.web.client.resources;

import com.google.gwt.core.client.GWT;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.ImageResource;

public interface GlobalResources extends ClientBundle {

    public static final GlobalResources RESOURCE = GWT.create(GlobalResources.class);

    @Source("images/istacLogo.gif")
    ImageResource istacLogo();

    @Source("images/new_listgrid.png")
    ImageResource newListGrid();

    @Source("images/edit_listgrid.png")
    ImageResource editListGrid();

    @Source("images/save_listgrid.png")
    ImageResource saveListGrid();

    @Source("images/delete_listgrid.png")
    ImageResource deleteListGrid();

    @Source("images/cancel_listgrid.png")
    ImageResource cancelListGrid();

    @Source("images/info.png")
    ImageResource info();

    @Source("images/error.png")
    ImageResource error();

    @Source("images/ask.png")
    ImageResource ask();

    @Source("images/success.png")
    ImageResource success();

    @Source("images/publish_internally.png")
    ImageResource publishInternally();

    @Source("images/publish_externally.png")
    ImageResource publishExternally();

    @Source("images/unpublish_externally.png")
    ImageResource unpublishExternally();

}
